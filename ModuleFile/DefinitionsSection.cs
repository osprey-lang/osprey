using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.TypeDefStruct();
			data.Flags = GetTypeFlags();
			data.Name = new MetadataToken(Type.Module.GetStringId(Type.FullName));
			data.BaseType = Type.BaseType != null
				? new MetadataToken(Type.BaseType.Id)
				: MetadataToken.Null;
			data.SharedType = Type.SharedType != null
				? new MetadataToken(Type.SharedType.Id)
				: MetadataToken.Null;

			data.Annotations = 0u; // not supported

			if (Initer != null)
				data.Initer = Initer.ToRva<Raw.ByteStringStruct>();

			if (Fields != null && Fields.Length > 0)
			{ 
				data.FieldCount = Fields.Length;
				data.FirstField = new MetadataToken(Fields[0].Token);
			}

			if (Methods != null && Methods.Length > 0)
			{
				data.MethodCount = Methods.Length;
				data.FirstMethod = new MetadataToken(Methods[0].Token);
			}

			if (Properties != null && Properties.Length > 0)
			{
				data.PropertyCount = Properties.Length;
				data.Properties = new Raw.RvaToArray<Raw.PropertyDefStruct>(Properties[0].Address);
			}

			if (Operators != null && Operators.Length > 0)
			{
				data.OperatorCount = Operators.Length;
				data.Operators = new Raw.RvaToArray<Raw.OperatorDefStruct>(Operators[0].Address);
			}

			view.Write(this.Address, ref data);
		}

		private Raw.TypeFlags GetTypeFlags()
		{
			var @class = Type as Members.Class;
			if (@class != null)
				return GetClassFlags(@class);
			else
				return GetEnumFlags((Members.Enum)Type);
		}

		private static Raw.TypeFlags GetClassFlags(Members.Class type)
		{
			var flags = (Raw.TypeFlags)0;

			if (type.Access == AccessLevel.Public)
				flags |= Raw.TypeFlags.Public;
			else
				flags |= Raw.TypeFlags.Internal;

			if (type.IsStatic)
				flags |= Raw.TypeFlags.Static;
			else if (type.IsAbstract)
				flags |= Raw.TypeFlags.Abstract;
			else if (!type.IsInheritable)
				flags |= Raw.TypeFlags.Sealed;

			if (type is Members.GeneratorClass || type is Members.ClosureClass)
				flags |= Raw.TypeFlags.Impl;

			if (type.IsPrimitive)
				flags |= Raw.TypeFlags.Primitive;

			return flags;
		}

		private static Raw.TypeFlags GetEnumFlags(Members.Enum type)
		{
			var flags = (Raw.TypeFlags)0;

			// Enums are always primitive, which also requires
			// the type to be sealed.
			flags |= Raw.TypeFlags.Primitive;
			flags |= Raw.TypeFlags.Sealed;

			if (type.Access == AccessLevel.Public)
				flags |= Raw.TypeFlags.Public;
			else
				flags |= Raw.TypeFlags.Internal;

			return flags;
		}
	}

	public abstract class PropertyDef : FileObject
	{
		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class SimplePropertyDef : PropertyDef
	{
		public SimplePropertyDef(Members.Property property)
		{
			if (property.Parent.Module.Imported)
				throw new ArgumentException("The property belongs to an imported, not declared, type.", "property");
			this.Property = property;
		}

		public readonly Members.Property Property;

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.PropertyDefStruct();
			data.Name = new MetadataToken(Property.Parent.Module.GetStringId(Property.Name));
			data.Getter = new MetadataToken(Property.GetterId);
			data.Setter = new MetadataToken(Property.SetterId);
			view.Write(this.Address, ref data);
		}
	}

	public class IndexerPropertyDef : PropertyDef
	{
		public IndexerPropertyDef(Members.IndexerMember indexer)
		{
			if (indexer.Parent.Module.Imported)
				throw new ArgumentException("The indexer belongs to an imported, not declared, type.", "indexer");
			this.Indexer = indexer;
		}

		public readonly Members.IndexerMember Indexer;

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.PropertyDefStruct();
			data.Name = new MetadataToken(Indexer.Parent.Module.GetStringId(Indexer.Name));
			data.Getter = new MetadataToken(Indexer.GetterGroupId);
			data.Setter = new MetadataToken(Indexer.SetterGroupId);
		}
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.OperatorDefStruct();
			data.Operator = (Raw.Operator)Operator.Index;
			data.Method = new MetadataToken(Operator.Method.Group.Id);
			view.Write(this.Address, ref data);
		}
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.FieldDefStruct();
			data.Flags = GetFieldFlags();
			data.Name = new MetadataToken(Field.Parent.Module.GetStringId(Field.Name));
			data.DeclType = new MetadataToken(Field.Parent.Id);
			data.Annotations = 0u; // not supported
			data.Value = Raw.Rva<Raw.ConstantValueStruct>.Null;
			view.Write(this.Address, ref data);
		}

		private Raw.FieldFlags GetFieldFlags()
		{
			var flags = (Raw.FieldFlags)0;

			switch (Field.Access)
			{
				case AccessLevel.Public:
					flags |= Raw.FieldFlags.Public;
					break;
				case AccessLevel.Protected:
					flags |= Raw.FieldFlags.Protected;
					break;
				default:
					flags |= Raw.FieldFlags.Private;
					break;
			}

			if (!Field.IsStatic)
				flags |= Raw.FieldFlags.Instance;
			if (Field.IsImplDetail)
				flags |= Raw.FieldFlags.Impl;

			return flags;
		}
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.FieldDefStruct();
			data.Flags = GetFieldFlags();
			data.Name = new MetadataToken(Constant.Parent.Module.GetStringId(Constant.Name));
			data.DeclType = new MetadataToken(Constant.Parent.Id);
			data.Annotations = 0u; // not supported
			data.Value = Value.ToRva<Raw.ConstantValueStruct>();
			view.Write(this.Address, ref data);
		}

		private Raw.FieldFlags GetFieldFlags()
		{
			var flags = (Raw.FieldFlags)0;

			switch (Constant.Access)
			{
				case AccessLevel.Public:
					flags |= Raw.FieldFlags.Public;
					break;
				case AccessLevel.Protected:
					flags |= Raw.FieldFlags.Protected;
					break;
				default:
					flags |= Raw.FieldFlags.Private;
					break;
			}

			flags |= Raw.FieldFlags.HasValue;
			if (Constant.IsImplDetail)
				flags |= Raw.FieldFlags.Impl;

			return flags;
		}
	}

	public class EnumFieldDef : FieldDef
	{
		public EnumFieldDef(Members.EnumField field, ConstantValueObject value)
		{
			this.Field = field;
			this.Value = value;
		}

		public override uint Token { get { return Field.Id; } }

		public readonly Members.EnumField Field;
		public readonly ConstantValueObject Value;

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.FieldDefStruct();
			data.Flags = GetFieldFlags();
			data.Name = new MetadataToken(Field.Parent.Module.GetStringId(Field.Name));
			data.DeclType = new MetadataToken(Field.Parent.Id);
			data.Annotations = 0u; // not supported
			data.Value = Value.ToRva<Raw.ConstantValueStruct>();
			view.Write(this.Address, ref data);
		}

		private Raw.FieldFlags GetFieldFlags()
		{
			// all enum fields have the same flags
			return Raw.FieldFlags.Public | Raw.FieldFlags.HasValue;
		}
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.MethodDefStruct();
			data.Flags = GetMethodFlags();

			if (Method.ParentAsClass != null)
			{
				data.Name = new MetadataToken(Method.Module.GetStringId(Method.Name));
				data.DeclType = new MetadataToken(Method.ParentAsClass.Id);
			}
			else
			{
				data.Name = new MetadataToken(Method.Module.GetStringId(Method.FullName));
			}

			data.OverloadCount = Overloads.Length;
			data.Overloads = new Raw.RvaToArray<Raw.OverloadDefStruct>(Overloads[0].Address);

			view.Write(this.Address, ref data);
		}

		private Raw.MethodFlags GetMethodFlags()
		{
			var flags = (Raw.MethodFlags)0;

			var parentClass = Method.ParentAsClass;
			switch (Method.Access)
			{
				case AccessLevel.Public:
					flags |= Raw.MethodFlags.Public;
					break;
				case AccessLevel.Protected:
					flags |= Raw.MethodFlags.Protected;
					break;
				default:
					// On global functions, private == internal
					if (parentClass != null)
						flags |= Raw.MethodFlags.Private;
					else
						flags |= Raw.MethodFlags.Internal;
					break;
			}

			if (!Method.IsStatic)
				flags |= Raw.MethodFlags.Instance;
			if (Method.Any(o => o.IsImplDetail))
				flags |= Raw.MethodFlags.Impl;
			if (parentClass != null &&
				(parentClass.Constructors == Method || parentClass.StaticConstructor == Method))
				flags |= Raw.MethodFlags.Ctor;

			return flags;
		}
	}

	public class OverloadDef : FileObject
	{
		public OverloadDef(Members.Method overload, Parameter[] parameters, MethodBody body)
		{
			if (overload.Group.Module.Imported)
				throw new ArgumentException("The containing method is imported, not declared.", "overload");

			this.Overload = overload;
			this.Parameters = parameters;
			this.Body = body;
		}

		public readonly Members.Method Overload;
		public readonly Parameter[] Parameters;
		public readonly MethodBody Body;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.OverloadDefStruct();
			data.Flags = GetOverloadFlags();
			data.Annotations = 0u; // not supported
			if (Parameters != null && Parameters.Length > 0)
			{
				data.ParamCount = Parameters.Length;
				data.Params = new Raw.RvaToArray<Raw.ParameterStruct>(Parameters[0].Address);
			}

			if (Body != null)
			{
				// The most correct thing to do here would be to test the type of Body against
				// the known MethodBody derivatives and call ToRva() with the right struct type
				// for data.LongHeader, data.ShortHeader and data.NativeHeader. However, all of
				// these fields overlap, and are all RVAs. In practice the result is identical
				// anyway, so let's do the ugly but very marginally faster thing...
				data.LongHeader = Body.ToRva<Raw.MethodHeaderStruct>();
			}

			view.Write(this.Address, ref data);
		}

		private Raw.OverloadFlags GetOverloadFlags()
		{
			var flags = (Raw.OverloadFlags)0;

			if (Overload.Signature.Splat != Nodes.Splat.None)
				flags |= Raw.OverloadFlags.Variadic;

			if (Overload.IsOverridable)
				flags |= Raw.OverloadFlags.Virtual;
			else if (Overload.IsAbstract)
				flags |= Raw.OverloadFlags.Abstract;

			if (Overload.IsOverride)
				flags |= Raw.OverloadFlags.Override;

			if (Body is ShortMethodBody)
				flags |= Raw.OverloadFlags.ShortHeader;
			else if (Body is NativeMethodBody)
				flags |= Raw.OverloadFlags.Native;

			return flags;
		}
	}

	public class Parameter : FileObject
	{
		public Parameter(Nodes.Parameter parameter, uint nameToken)
		{
			this.ParameterNode = parameter;
			this.NameToken = nameToken;
		}

		public readonly Nodes.Parameter ParameterNode;
		public readonly uint NameToken;

		public override uint Size { get { return 8; } }

		public override uint Alignment { get { return 8; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.ParameterStruct();
			data.Flags = GetParamFlags();
			data.Name = new MetadataToken(NameToken);
			view.Write(this.Address, ref data);
		}

		private Raw.ParamFlags GetParamFlags()
		{
			var flags = (Raw.ParamFlags)0;

			if (ParameterNode.DefaultValue != null)
				flags |= Raw.ParamFlags.Optional;
			else if (ParameterNode.IsByRef)
				flags |= Raw.ParamFlags.ByRef;

			return flags;
		}
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.ConstantDefStruct();
			data.Flags = GetConstantFlags();
			data.Name = new MetadataToken(Constant.Module.GetStringId(Constant.FullName));
			data.Annotations = 0u; // not supported
			data.Value = Value.ToRva<Raw.ConstantValueStruct>();
		}

		private Raw.ConstantFlags GetConstantFlags()
		{
			var flags = (Raw.ConstantFlags)0;

			if (Constant.Access == AccessLevel.Public)
				flags |= Raw.ConstantFlags.Public;
			else
				flags |= Raw.ConstantFlags.Internal;

			return flags;
		}
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

			allChildren = new FileSectionArray<FileObject>(this, 9)
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

		private readonly FileSectionArray<FileObject> allChildren;

		public override uint Size
		{
			get
			{
				return allChildren.AlignedSize;
			}
		}

		public override uint Alignment { get { return 16; } }

		public override void LayOutChildren()
		{
			allChildren.RelativeAddress = 0;
			allChildren.LayOutChildren();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			allChildren.Emit(view);
		}
	}
}
