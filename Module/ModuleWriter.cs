using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.ModuleFile;

namespace Osprey
{
	public class ModuleWriter : IFileObjectFactory
	{
		public ModuleWriter()
		{
			allSections = new FileObjectArray<FileSection>(null, 6)
			{
				stringData,
				metadata,
				references,
				definitions,
				constants,
				methodBodies,
			};
		}

		private ModuleVersion moduleVersion;
		private WideString moduleName;
		private WideString nativeLibraryName;

		private StringDataSection stringData = new StringDataSection();
		private MetadataSection metadata = new MetadataSection();
		private ReferencesSection references = new ReferencesSection();
		private DefinitionsSection definitions = new DefinitionsSection();
		private ConstantPool constants = new ConstantPool();
		private MethodBodySection methodBodies = new MethodBodySection();
		private FileObjectArray<FileSection> allSections;

		public WideString GetWideString(string value)
		{
			// All wide strings are "pre-added", so should be in the string section already
			return stringData.GetWideString(value);
		}

		public ByteString GetByteString(string value)
		{
			// Byte strings are added on demand
			return stringData.AddByteString(value);
		}

		public void AddBasicModuleData(Module module)
		{
			// Module strings must be added first, because there are compiled methods that
			// rely on specific string tokens.
			foreach (var kvp in module.Members.Strings)
				stringData.AddString(kvp.Value);

			// In the old module format, some strings did not have string tokens associated
			// with them; the String struct was inlined. Since the new format is basically
			// bolted onto an old compiler that was definitely not made for it, those strings
			// are not in the usual string table, so we must add them separately.
			//
			// Note that these strings are not referred to by token, only by RVA.

			moduleName = stringData.AddString(module.Name);
			if (module.NativeLib != null)
				nativeLibraryName = stringData.AddString(module.NativeLib);

			foreach (var kvp in module.Metadata)
				metadata.AddEntry(
					stringData.AddString(kvp.Key),
					stringData.AddString(kvp.Value)
				);

			moduleVersion = new ModuleVersion(
				module.Version.Major,
				module.Version.Minor,
				module.Version.Revision
			);
		}

		public TypeDef CreateTypeDef(Members.Type type)
		{
			var @class = type as Members.Class;
			if (@class != null)
				return CreateClassDef(@class);
			else
				return CreateEnumDef((Members.Enum)type);
		}

		public TypeDef CreateClassDef(Members.Class type)
		{
			var fields = new TempList<FieldDef>(type.members.Count / 2);
			var methods = new TempList<MethodDef>(type.members.Count / 2);
			var properties = new TempList<PropertyDef>(type.members.Count / 2);

			foreach (var member in type.members.Values)
			{
				switch (member.Kind)
				{
					case Members.MemberKind.Constructor:
					case Members.MemberKind.Iterator:
						// The implementing method will be visited eventually
						continue;
					case Members.MemberKind.Field:
						fields.Add(CreateClassFieldDef((Members.Field)member));
						break;
					case Members.MemberKind.Constant:
						fields.Add(CreateClassConstantDef((Members.ClassConstant)member));
						break;
					case Members.MemberKind.MethodGroup:
						methods.Add(CreateMethodDef((Members.MethodGroup)member));
						break;
					case Members.MemberKind.Property:
						properties.Add(CreateProperty((Members.Property)member));
						break;
					case Members.MemberKind.IndexerMember:
						properties.Add(CreateProperty((Members.IndexerMember)member));
						break;
					default:
						throw new CompileTimeException(member.Node, "Unsupported member in class");
				}
			}

			var initer = type.Initializer != null
				? stringData.AddByteString(type.Initializer)
				: null;
			var operators =
				type.operators.Where(op => op != null)
					.Select(CreateOperator);

			// Fields and methods must be sorted by token value
			var fieldsArray = fields.ToArray();
			Array.Sort(fieldsArray, FieldComparer);
			var methodsArray = methods.ToArray();
			Array.Sort(methodsArray, MethodComparer);

			var result = new TypeDef(
				type,
				initer,
				fieldsArray,
				methodsArray,
				properties.ToArray(),
				operators.ToArray()
			);
			definitions.TypeDefs.Add(result);
			return result;
		}

		public TypeDef CreateEnumDef(Members.Enum type)
		{
			// fields have to be in token value order.
			var fields = type.members.Values
				.OrderBy(m => m.Id)
				.Select(CreateEnumFieldDef)
				.ToArray();
			var result = new TypeDef(type, null, fields, null, null, null);
			definitions.TypeDefs.Add(result);
			return result;
		}

		public PropertyDef CreateProperty(Members.Property property)
		{
			var result = new SimplePropertyDef(property);
			definitions.PropertyDefs.Add(result);
			return result;
		}

		public PropertyDef CreateProperty(Members.IndexerMember indexer)
		{
			var result = new IndexerPropertyDef(indexer);
			definitions.PropertyDefs.Add(result);
			return result;
		}

		public OperatorDef CreateOperator(Members.OperatorOverload @operator)
		{
			var result = new OperatorDef(@operator);
			definitions.OperatorDefs.Add(result);
			return result;		}


		public ClassFieldDef CreateClassFieldDef(Members.Field field)
		{
			var result = new ClassFieldDef(field);
			definitions.FieldDefs.Add(result);
			return result;
		}

		public ClassConstantDef CreateClassConstantDef(Members.ClassConstant constant)
		{
			var value = constants.Add(constant.Value);
			return new ClassConstantDef(constant, value);
		}

		public EnumFieldDef CreateEnumFieldDef(Members.EnumField field)
		{
			var result = new EnumFieldDef(field);
			definitions.FieldDefs.Add(result);
			return result;
		}

		public MethodDef CreateMethodDef(Members.MethodGroup method)
		{
			var overloads = method
				.Select(CreateOverloadDef)
				.ToArray();
			var result = new MethodDef(method, overloads);
			if (method.ParentAsClass == null)
				this.definitions.FunctionDefs.Add(result);
			else
				this.definitions.MethodDefs.Add(result);
			return result;
		}

		public OverloadDef CreateOverloadDef(Members.Method overload)
		{
			var parameters = overload.Parameters != null
				? overload.Parameters.Select(CreateParameter).ToArray()
				: null;
			var body = overload.IsAbstract ? null : CreateMethodBody(overload);
			var result = new OverloadDef(overload, parameters, body);
			definitions.OverloadDefs.Add(result);
			return result;
		}

		private MethodBody CreateMethodBody(Members.Method overload)
		{
			var compiledMethod = overload.CompiledMethod;

			var externBody = overload.Body as Members.ExternBlockSpace;
			MethodBody result;
			if (externBody != null)
				result = CreateNativeMethodBody(compiledMethod.LocalCount, externBody.EntryPoint);
			else if (CanUseShortHeader(overload))
				result = CreateShortMethodBody(overload);
			else
				result = CreateLongMethodBody(overload);
			return methodBodies.Add(result);
		}

		private NativeMethodBody CreateNativeMethodBody(int localCount, string entryPoint)
		{
			var entryPointString = GetByteString(entryPoint);
			return new NativeMethodBody(localCount, entryPointString);
		}

		private bool CanUseShortHeader(Members.Method overload)
		{
			var compiledMethod = overload.CompiledMethod;
			return compiledMethod.LocalCount == 0 &&
				compiledMethod.MaxStack <= 8 &&
				(compiledMethod.TryBlocks == null || compiledMethod.TryBlocks.Length == 0);
		}

		private ShortMethodBody CreateShortMethodBody(Members.Method overload)
		{
			return new ShortMethodBody(overload);
		}

		private LongMethodBody CreateLongMethodBody(Members.Method overload)
		{
			return new LongMethodBody(overload);
		}

		public Parameter CreateParameter(Nodes.Parameter parameter)
		{
			var result = new Parameter(parameter);
			definitions.Parameters.Add(result);
			return result;
		}

		public ConstantDef CreateConstantDef(Members.GlobalConstant constant)
		{
			var value = constants.Add(constant.Value);
			return new ConstantDef(constant, value);
		}

		public ModuleRef CreateModuleRef(Module module)
		{
			var result = new ModuleRef(module);
			references.ModuleRefs.Add(result);
			return result;
		}

		public TypeRef CreateTypeRef(Members.Type type)
		{
			var result = new TypeRef(type);
			references.TypeRefs.Add(result);
			return result;
		}

		public FieldRef CreateFieldRef(Members.Field field)
		{
			var result = new FieldRef(field);
			references.FieldRefs.Add(result);
			return result;
		}

		public MethodRef CreateMethodRef(Members.MethodGroup method)
		{
			var result = new MethodRef(method);
			references.MethodRefs.Add(result);
			return result;
		}

		public FunctionRef CreateFunctionRef(Members.MethodGroup function)
		{
			var result = new FunctionRef(function);
			references.FunctionRefs.Add(result);
			return result;
		}

		private static Comparison<FieldDef> FieldComparer = (a, b) => a.Token.CompareTo(b.Token);
		private static Comparison<MethodDef> MethodComparer = (a, b) => a.Token.CompareTo(b.Token);
	}
}