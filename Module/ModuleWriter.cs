using System;
using System.Collections.Generic;
using System.IO;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;
using Osprey.ModuleFile;
using Raw = Osprey.ModuleFile.Raw;

namespace Osprey
{
	public class ModuleWriter : IFileObjectFactory
	{
		public ModuleWriter()
		{
			allSections = new FileSectionArray<FileSection>(null, 6)
			{
				stringData,
				metadata,
				references,
				definitions,
				constants,
				methodBodies,
			};
		}

		private Module module;
		private ModuleVersion moduleVersion;
		private WideString moduleName;
		private WideString nativeLibraryName;

		private StringDataSection stringData = new StringDataSection();
		private MetadataSection metadata = new MetadataSection();
		private ReferencesSection references = new ReferencesSection();
		private DefinitionsSection definitions = new DefinitionsSection();
		private ConstantPool constants = new ConstantPool();
		private MethodBodySection methodBodies = new MethodBodySection();
		private FileSectionArray<FileSection> allSections;

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
			this.module = module;

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

			foreach (var member in type.GetMembersSorted())
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

			var result = new TypeDef(
				type,
				initer,
				fields.ToArray(),
				methods.ToArray(),
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
			return result;
		}

		public ClassFieldDef CreateClassFieldDef(Members.Field field)
		{
			var result = new ClassFieldDef(field);
			definitions.FieldDefs.Add(result);
			return result;
		}

		public ClassConstantDef CreateClassConstantDef(Members.ClassConstant constant)
		{
			var value = CreateConstantValue(constant.Value);
			var result = new ClassConstantDef(constant, value);
			definitions.FieldDefs.Add(result);
			return result;
		}

		public EnumFieldDef CreateEnumFieldDef(Members.EnumField field)
		{
			var value = CreateConstantValue(field.Value);
			var result = new EnumFieldDef(field, value);
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
			var entryPointString = new ByteString(entryPoint);
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
			var nameToken = module.GetStringId(parameter.Name);
			var result = new Parameter(parameter, nameToken);
			definitions.Parameters.Add(result);
			return result;
		}

		public ConstantDef CreateConstantDef(Members.GlobalConstant constant)
		{
			var value = CreateConstantValue(constant.Value);
			return new ConstantDef(constant, value);
		}

		public ModuleRef CreateModuleRef(Module module)
		{
			var nameToken = this.module.GetStringId(module.Name);
			var result = new ModuleRef(module, nameToken);
			references.ModuleRefs.Add(result);
			return result;
		}

		public TypeRef CreateTypeRef(Members.Type type)
		{
			var nameToken = this.module.GetStringId(type.FullName);
			var declModuleToken = this.module.Members.ModuleRefs.GetId(type.Module);
			var result = new TypeRef(nameToken, declModuleToken);
			references.TypeRefs.Add(result);
			return result;
		}

		public FieldRef CreateFieldRef(Members.Field field)
		{
			var nameToken = this.module.GetStringId(field.Name);
			var result = new FieldRef(field.Parent.Id, nameToken);
			references.FieldRefs.Add(result);
			return result;
		}

		public MethodRef CreateMethodRef(Members.MethodGroup method)
		{
			var nameToken = this.module.GetStringId(method.Name);
			var result = new MethodRef(method.ParentAsClass.Id, nameToken);
			references.MethodRefs.Add(result);
			return result;
		}

		public FunctionRef CreateFunctionRef(Members.MethodGroup function)
		{
			var declModuleToken = this.module.Members.ModuleRefs.GetId(function.Module);
			var nameToken = this.module.GetStringId(function.FullName);
			var result = new FunctionRef(declModuleToken, nameToken);
			references.FunctionRefs.Add(result);
			return result;
		}

		private ConstantValueObject CreateConstantValue(ConstantValue value)
		{
			uint typeToken = 0u;
			uint stringToken = 0u;
			switch (value.Type)
			{
				case ConstantValueType.Null:
					// everything is zero!
					break;
				case ConstantValueType.Boolean:
				case ConstantValueType.Int:
				case ConstantValueType.UInt:
				case ConstantValueType.Real:
				case ConstantValueType.Char:
					typeToken = value.GetTypeObject(module.Pool.Compiler).Id;
					break;
				case ConstantValueType.String:
					typeToken = value.GetTypeObject(module.Pool.Compiler).Id;
					stringToken = module.GetStringId(value.StringValue);
					break;
				case ConstantValueType.Enum:
					typeToken = value.EnumValue.Type.Id;
					break;
				default:
					throw new InvalidOperationException("Invalid ConstantValueType");
			}

			return constants.Add(value, typeToken, stringToken);
		}

		public uint LayOutSections()
		{
			// The RefTableHeader begins right after the ModuleHeader,
			// after which comes all that juicy data.
			allSections.RelativeAddress = ModuleHeaderSize + RefTableHeaderSize;
			allSections.LayOutChildren();

			return allSections.Address + allSections.Size;
		}

		public void Emit(MemoryMappedFile file)
		{
			using (var view = file.CreateViewAccessor())
			{
				EmitHeader(view);

				allSections.Emit(view);
			}
		}

		private void EmitHeader(MemoryMappedViewAccessor view)
		{
			// RefTableHeader is placed right after the ModuleHeader
			var refTableAddress = ModuleHeaderSize;
			EmitModuleHeader(view, refTableAddress);
			EmitRefTableHeader(view, refTableAddress);
		}

		private void EmitModuleHeader(MemoryMappedViewAccessor view, uint refTableAddress)
		{
			var header = new Raw.ModuleHeaderStruct();
			header.Magic = Module.MagicNumber;
			header.FormatVersion = Module.MaxFileFormatVersion;

			header.Version = new Raw.ModuleVersionStruct
			{
				Major = unchecked((uint)moduleVersion.Major),
				Minor = unchecked((uint)moduleVersion.Minor),
				Patch = unchecked((uint)moduleVersion.Patch),
			};
			header.Name = moduleName.ToRva<Raw.StringStruct>();

			header.Strings = stringData.ToRva<Raw.StringTableHeaderStruct>();

			header.NativeLib = nativeLibraryName != null
				? nativeLibraryName.ToRva<Raw.StringStruct>()
				: Raw.Rva<Raw.StringStruct>.Null;

			header.References = new Raw.Rva<Raw.RefTableHeaderStruct>(refTableAddress);

			header.Metadata = metadata.ToRva<Raw.StringMapHeaderStruct>();

			header.MainMethod = module.MainMethod != null ? module.MainMethod.Id : 0u;

			header.Types = definitions.TypeDefs.ToRva<Raw.TypeDefStruct>(out header.TypeCount);
			header.Fields = definitions.FieldDefs.ToRva<Raw.FieldDefStruct>(out header.FieldCount);
			header.Methods = definitions.MethodDefs.ToRva<Raw.MethodDefStruct>(out header.MethodCount);
			header.Functions = definitions.FunctionDefs.ToRva<Raw.MethodDefStruct>(out header.FunctionCount);
			header.Constants = definitions.ConstantDefs.ToRva<Raw.ConstantDefStruct>(out header.ConstantCount);

			header.Annotations = 0; // not supported

			view.Write(0, ref header);
		}

		private void EmitRefTableHeader(MemoryMappedViewAccessor view, uint offset)
		{
			var header = new Raw.RefTableHeaderStruct();
			header.ModuleRefs = references.ModuleRefs.ToRva<Raw.ModuleRefStruct>(out header.ModuleRefCount);
			header.TypeRefs = references.TypeRefs.ToRva<Raw.TypeRefStruct>(out header.TypeRefCount);
			header.FieldRefs = references.FieldRefs.ToRva<Raw.FieldRefStruct>(out header.FieldRefCount);
			header.MethodRefs = references.MethodRefs.ToRva<Raw.MethodRefStruct>(out header.MethodRefCount);
			header.FunctionRefs = references.FunctionRefs.ToRva<Raw.FunctionRefStruct>(out header.FunctionRefCount);

			view.Write(offset, ref header);
		}

		private const uint ModuleHeaderSize = 96u;
		private const uint RefTableHeaderSize = 40u;

		private static Comparison<FieldDef> FieldComparer = (a, b) => a.Token.CompareTo(b.Token);
		private static Comparison<MethodDef> MethodComparer = (a, b) => a.Token.CompareTo(b.Token);
	}
}