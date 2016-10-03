using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class ModuleRef : FileObject
	{
		public ModuleRef(Module module, uint nameToken)
		{
			if (!module.Imported)
				throw new ArgumentException("The module is not imported.", "module");
			this.Module = module;
			this.NameToken = nameToken;
		}

		public readonly Module Module;
		public readonly uint NameToken;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.ModuleRefStruct();
			data.Name = new MetadataToken(NameToken);
			data.VersionConstraint = Raw.VersionConstraint.Exact;
			data.Version = new Raw.ModuleVersionStruct
			{
				Major = (uint)Module.Version.Major,
				Minor = (uint)Module.Version.Minor,
				Patch = (uint)Module.Version.Revision,
			};
			view.Write(this.Address, ref data);
		}
	}

	public class TypeRef : FileObject
	{
		public TypeRef(uint nameToken, uint declModuleToken)
		{
			this.NameToken = nameToken;
			this.DeclModuleToken = declModuleToken;
		}

		public readonly uint NameToken;
		public readonly uint DeclModuleToken;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.TypeRefStruct();
			data.DeclModule = new MetadataToken(DeclModuleToken);
			data.Flags = Raw.TypeRefFlags.None;
			data.Name = new MetadataToken(NameToken);
			view.Write(this.Address, ref data);
		}
	}

	public class FieldRef : FileObject
	{
		public FieldRef(uint declTypeToken, uint nameToken)
		{
			this.DeclTypeToken = declTypeToken;
			this.NameToken = nameToken;
		}

		public readonly uint DeclTypeToken;
		public readonly uint NameToken;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.FieldRefStruct();
			data.DeclType = new MetadataToken(DeclTypeToken);
			data.Flags = Raw.FieldRefFlags.None;
			data.Name = new MetadataToken(NameToken);
			view.Write(this.Address, ref data);
		}
	}

	public class MethodRef : FileObject
	{
		public MethodRef(uint declTypeToken, uint nameToken)
		{
			this.DeclTypeToken = declTypeToken;
			this.NameToken = nameToken;
		}

		public readonly uint DeclTypeToken;
		public readonly uint NameToken;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.MethodRefStruct();
			data.DeclType = new MetadataToken(DeclTypeToken);
			data.Flags = Raw.MethodRefFlags.None;
			data.Name = new MetadataToken(NameToken);
			view.Write(this.Address, ref data);
		}
	}

	public class FunctionRef : FileObject
	{
		public FunctionRef(uint declModuleToken, uint nameToken)
		{
			this.DeclModuleToken = declModuleToken;
			this.NameToken = nameToken;
		}

		public readonly uint DeclModuleToken;
		public readonly uint NameToken;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var data = new Raw.FunctionRefStruct();
			data.DeclModule = new MetadataToken(DeclModuleToken);
			data.Flags = Raw.FunctionRefFlags.None;
			data.Name = new MetadataToken(NameToken);
			view.Write(this.Address, ref data);
		}
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

			allChildren = new FileSectionArray<FileObject>(this, 5)
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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			// The actual RefTableHeader is emitted by ModuleWriter, because it's always
			// placed immediately after the ModuleHeader. So we only need to concern
			// ourselves with emitting the actual members here.
			allChildren.Emit(view);
		}
	}
}
