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
			return new PropertyDef(property);
		}

		public OperatorDef CreateOperator(Members.OperatorOverload @operator)
		{
			return new OperatorDef(@operator);
		}

		public ClassFieldDef CreateClassFieldDef(Members.Field field)
		{
			return new ClassFieldDef(field);
		}

		public EnumFieldDef CreateEnumFieldDef(Members.EnumField field)
		{
			return new EnumFieldDef(field);
		}

		public MethodDef CreateMethodDef(Members.MethodGroup method)
		{
			var overloads = method
				.Select(CreateOverloadDef)
				.ToArray();
			throw new NotImplementedException();
		}

		public OverloadDef CreateOverloadDef(Members.Method overload)
		{
			var parameters = overload.Parameters.Select(CreateParameter);
			var body = CreateMethodBody(overload);
			return new OverloadDef(overload, body);
		}

		private MethodBody CreateMethodBody(Members.Method overload)
		{
			var compiledMethod = overload.CompiledMethod;

			var externBody = overload.Body as Members.ExternBlockSpace;
			if (externBody != null)
				return CreateNativeMethodBody(compiledMethod.LocalCount, externBody.EntryPoint);
			else if (CanUseShortHeader(overload))
				return CreateShortMethodBody(overload);
			else
				return CreateLongMethodBody(overload);
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
			return new Parameter(parameter);
		}

		public ModuleRef CreateModuleRef(Module module)
		{
			return new ModuleRef(module);
		}

		public TypeRef CreateTypeRef(Members.Type type)
		{
			return new TypeRef(type);
		}

		public FieldRef CreateFieldRef(Members.Field field)
		{
			return new FieldRef(field);
		}

		public MethodRef CreateMethodRef(Members.MethodGroup method)
		{
			return new MethodRef(method);
		}

		public FunctionRef CreateFunctionRef(Members.MethodGroup function)
		{
			return new FunctionRef(function);
		}
	}
}