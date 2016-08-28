using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public interface IFileObjectFactory
	{
		TypeDef CreateClassDef(Members.Class type);

		TypeDef CreateEnumDef(Members.Enum type);

		PropertyDef CreateProperty(Members.Property property);

		OperatorDef CreateOperator(Members.OperatorOverload @operator);

		ClassFieldDef CreateClassFieldDef(Members.Field field);

		EnumFieldDef CreateEnumFieldDef(Members.EnumField field);

		MethodDef CreateMethodDef(Members.MethodGroup method);

		OverloadDef CreateOverloadDef(Members.Method overload);

		Parameter CreateParameter(Nodes.Parameter parameter);

		ModuleRef CreateModuleRef(Module module);

		TypeRef CreateTypeRef(Members.Type type);

		FieldRef CreateFieldRef(Members.Field field);

		MethodRef CreateMethodRef(Members.MethodGroup method);

		FunctionRef CreateFunctionRef(Members.MethodGroup function);
	}

	public class FileObjectFactory : IFileObjectFactory
	{
		public FileObjectFactory(ModuleWriter writer)
		{
			this.writer = writer;
		}

		private ModuleWriter writer;

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
			throw new NotImplementedException();
		}

		public OverloadDef CreateOverloadDef(Members.Method overload)
		{
			throw new NotImplementedException();
		}

		public Parameter CreateParameter(Members.Variable variable)
		{
			return new Parameter(variable);
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
