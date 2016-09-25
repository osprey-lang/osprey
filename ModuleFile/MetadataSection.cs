using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class MetadataEntry : FileObject
	{
		public MetadataEntry(WideString key, WideString value)
		{
			this.Key = key;
			this.Value = value;
		}

		private readonly WideString Key;
		private readonly WideString Value;

		public override uint Size { get { return 8; } }

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var entry = new Raw.StringMapEntryStruct();
			entry.Key = Key.ToRva<Raw.StringStruct>();
			entry.Value = Value.ToRva<Raw.StringStruct>();
			view.Write(this.Address, ref entry);
		}
	}

	public class MetadataSection : FileSection
	{
		public MetadataSection()
		{
			entries = new FileObjectArray<MetadataEntry>(this, 5);
		}

		private FileObjectArray<MetadataEntry> entries;

		public override uint Size
		{
			get
			{
				return MetadataHeaderSize + entries.Size;
			}
		}

		public override uint Alignment { get { return 4; } }

		public MetadataEntry AddEntry(WideString key, WideString value)
		{
			var entry = new MetadataEntry(key, value);

			entries.Add(entry);

			return entry;
		}

		public override void LayOutChildren()
		{
			entries.RelativeAddress = MetadataHeaderSize;
			entries.LayOutChildren();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var header = new Raw.StringMapHeaderStruct();
			header.Length = entries.Count;
			view.Write(this.Address, ref header);

			entries.Emit(view);
		}

		private const uint MetadataHeaderSize = 4u;
	}
}
