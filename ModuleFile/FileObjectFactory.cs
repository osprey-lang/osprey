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
}
