using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class TypeDef : FileObject
	{
		public TypeDef(
			Members.Type type, ByteString initer,
			FieldDef[] fields, MethodDef[] methods,
			PropertyDef[] properties, OperatorDef[] operators
		)
		{
			if (type.Module.Imported)
				throw new ArgumentException("The type is imported, not declared.", "type");
			this.Type = type;
			this.Initer = initer;
			this.Fields = fields;
			this.Methods = methods;
			this.Properties = properties;
			this.Operators = operators;
		}

		public uint Token { get { return Type.Id; } }

		public readonly Members.Type Type;
		public readonly ByteString Initer;
		public readonly FieldDef[] Fields;
		public readonly MethodDef[] Methods;
		public readonly PropertyDef[] Properties;
		public readonly OperatorDef[] Operators;

		public override uint Size { get { return 56; } }

		public override uint Alignment { get { return 8; } }
	}

	public class PropertyDef : FileObject
	{
		public PropertyDef(Members.Property property)
		{
			if (property.Parent.Module.Imported)
				throw new ArgumentException("The property belongs to an imported, not declared, type.", "property");
			this.Property = property;
		}

		public readonly Members.Property Property;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class OperatorDef : FileObject
	{
		public OperatorDef(Members.OperatorOverload @operator)
		{
			if (@operator.Parent.Module.Imported)
				throw new ArgumentException("The operator overload belogs to an imported, not declared, type.", "operator");
			this.Operator = @operator;
		}

		public readonly Members.OperatorOverload Operator;

		public override uint Size { get { return 8; } }

		public override uint Alignment { get { return 4; } }
	}

	public abstract class FieldDef : FileObject
	{
		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }

		public abstract uint Token { get; }
	}

	public class ClassFieldDef : FieldDef
	{
		public ClassFieldDef(Members.Field field)
		{
			this.Field = field;
		}

		public override uint Token { get { return Field.Id; } }

		public readonly Members.Field Field;
	}

	public class ClassConstantDef : FieldDef
	{
		public ClassConstantDef(Members.ClassConstant constant, ConstantValueObject value)
		{
			this.Constant = constant;
			this.Value = value;
		}

		public override uint Token { get { return Constant.Id; } }

		public readonly Members.ClassConstant Constant;
		public readonly ConstantValueObject Value;
	}

	public class EnumFieldDef : FieldDef
	{
		public EnumFieldDef(Members.EnumField field)
		{
			this.Field = field;
		}

		public override uint Token { get { return Field.Id; } }

		public readonly Members.EnumField Field;
	}

	public class MethodDef : FileObject
	{
		public MethodDef(Members.MethodGroup method, OverloadDef[] overloads)
		{
			if (method.Module.Imported)
				throw new ArgumentException("The method is imported, not declared.", "method");
			this.Method = method;
			this.Overloads = overloads;
		}

		public uint Token { get { return Method.Id; } }

		public readonly Members.MethodGroup Method;
		public readonly OverloadDef[] Overloads;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }
	}

	public class OverloadDef : FileObject
	{
		public OverloadDef(Members.Method overload, MethodBody body)
		{
			if (overload.Group.Module.Imported)
				throw new ArgumentException("The containing method is imported, not declared.", "overload");

			this.Overload = overload;
			this.Body = body;
		}

		public readonly Members.Method Overload;
		public readonly MethodBody Body;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }
	}

	public class Parameter : FileObject
	{
		public Parameter(Nodes.Parameter parameter)
		{
			this.ParameterNode = parameter;
		}

		public readonly Nodes.Parameter ParameterNode;

		public override uint Size { get { return 8; } }

		public override uint Alignment { get { return 8; } }
	}

	public class ConstantDef : FileObject
	{
		public ConstantDef(Members.GlobalConstant constant, ConstantValueObject value)
		{
			this.Constant = constant;
			this.Value = value;
		}

		public readonly Members.GlobalConstant Constant;
		public readonly ConstantValueObject Value;

		public override uint Size { get { return 16; } }

		public override uint Alignment { get { return 8; } }
	}

	public class DefinitionsSection : FileSection
	{
		public DefinitionsSection()
		{
			TypeDefs = new FileObjectArray<TypeDef>(this, 10);
			PropertyDefs = new FileObjectArray<PropertyDef>(this, 40);
			OperatorDefs = new FileObjectArray<OperatorDef>(this, 10);
			FieldDefs = new FileObjectArray<FieldDef>(this, 40);
			MethodDefs = new FileObjectArray<MethodDef>(this, 80);
			FunctionDefs = new FileObjectArray<MethodDef>(this, 10);
			ConstantDefs = new FileObjectArray<ConstantDef>(this, 5);
			OverloadDefs = new FileObjectArray<OverloadDef>(this, 100);
			Parameters = new FileObjectArray<Parameter>(this, 100);

			allChildren = new FileObject[]
			{
				TypeDefs,
				PropertyDefs,
				OperatorDefs,
				FieldDefs,
				MethodDefs,
				FunctionDefs,
				ConstantDefs,
				OverloadDefs,
				Parameters,
			};
		}

		public readonly FileObjectArray<TypeDef> TypeDefs;
		public readonly FileObjectArray<PropertyDef> PropertyDefs;
		public readonly FileObjectArray<OperatorDef> OperatorDefs;
		public readonly FileObjectArray<FieldDef> FieldDefs;
		public readonly FileObjectArray<MethodDef> MethodDefs;
		public readonly FileObjectArray<MethodDef> FunctionDefs;
		public readonly FileObjectArray<ConstantDef> ConstantDefs;
		public readonly FileObjectArray<OverloadDef> OverloadDefs;
		public readonly FileObjectArray<Parameter> Parameters;

		private readonly FileObject[] allChildren;

		public override uint Size
		{
			get
			{
				var lastChild = allChildren[allChildren.Length - 1];
				return lastChild.RelativeAddress + lastChild.AlignedSize;
			}
		}

		public override uint Alignment { get { return 16; } }

		public override void LayOutChildren()
		{
			LayOutItems(0, allChildren);
		}
	}
}
