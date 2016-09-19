using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class ModuleRef : FileObject
	{
		public ModuleRef(Module module)
		{
			if (!module.Imported)
				throw new ArgumentException("The module is not imported.", "module");
			this.Module = module;
		}

		public readonly Module Module;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }
	}

	public class TypeRef : FileObject
	{
		public TypeRef(Members.Type type)
		{
			if (!type.Module.Imported)
				throw new ArgumentException("The type is not imported.", "type");
			this.Type = type;
		}

		public readonly Members.Type Type;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class FieldRef : FileObject
	{
		public FieldRef(Members.Field field)
		{
			if (!field.Parent.Module.Imported)
				throw new ArgumentException("The field is not imported.", "field");
			this.Field = field;
		}

		public readonly Members.Field Field;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class MethodRef : FileObject
	{
		public MethodRef(Members.MethodGroup method)
		{
			if (!method.Module.Imported)
				throw new ArgumentException("The method is not imported.", "method");
			this.Method = method;
		}

		public readonly Members.MethodGroup Method;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class FunctionRef : FileObject
	{
		public FunctionRef(Members.MethodGroup function)
		{
			if (!function.Module.Imported)
				throw new ArgumentException("The function is not imported.", "function");
			this.Function = function;
		}

		public readonly Members.MethodGroup Function;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }
	}

	public class ReferencesSection : FileSection
	{
		public ReferencesSection()
		{
			ModuleRefs = new FileObjectArray<ModuleRef>(this, 5);
			TypeRefs = new FileObjectArray<TypeRef>(this, 10);
			FieldRefs = new FileObjectArray<FieldRef>(this, 5);
			MethodRefs = new FileObjectArray<MethodRef>(this, 20);
			FunctionRefs = new FileObjectArray<FunctionRef>(this, 5);

			allChildren = new FileSectionArray<FileObject>(5)
			{
				ModuleRefs,
				TypeRefs,
				FieldRefs,
				MethodRefs,
				FunctionRefs,
			};
		}

		public readonly FileObjectArray<ModuleRef> ModuleRefs;
		public readonly FileObjectArray<TypeRef> TypeRefs;
		public readonly FileObjectArray<FieldRef> FieldRefs;
		public readonly FileObjectArray<MethodRef> MethodRefs;
		public readonly FileObjectArray<FunctionRef> FunctionRefs;

		private readonly FileSectionArray<FileObject> allChildren;

		public override uint Size { get { return allChildren.Size; } }

		public override uint Alignment { get { return 4; } }

		public override void LayOutChildren()
		{
			allChildren.LayOutChildren();
		}
	}
}
