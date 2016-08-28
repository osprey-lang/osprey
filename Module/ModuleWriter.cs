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
		public ModuleWriter(Module module)
		{
			this.module = module;

			allSections = new FileObjectArray<FileSection>(null, 5)
			{
				stringData,
				metadata,
				references,
				definitions,
				methodBodies,
			};
		}

		private Module module;

		private StringDataSection stringData = new StringDataSection();
		private MetadataSection metadata = new MetadataSection();
		private ReferencesSection references = new ReferencesSection();
		private DefinitionsSection definitions = new DefinitionsSection();
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

		public TypeDef CreateClassDef(Members.Class type)
		{
			throw new NotImplementedException();
		}

		public TypeDef CreateEnumDef(Members.Enum type)
		{
			throw new NotImplementedException();
		}

		public PropertyDef CreateProperty(Members.Property property)
		{
			var result = new PropertyDef(property);
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
			this.definitions.MethodDefs.Add(result);
			return result;
		}

		public OverloadDef CreateOverloadDef(Members.Method overload)
		{
			var parameters = overload.Parameters.Select(CreateParameter);
			var body = CreateMethodBody(overload);
			var result = new OverloadDef(overload, body);
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
				compiledMethod.TryBlocks.Length == 0;
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
	}
}