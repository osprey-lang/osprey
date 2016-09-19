using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class MetadataEntry : FileObject
	{
		public MetadataEntry(WideString key, WideString value)
		{
			this.key = key;
			this.value = value;
		}

		private WideString key;
		public WideString Key { get { return key; } }

		private WideString value;
		public WideString Value { get { return value; } }

		public override uint Size { get { return 8; } }

		public override uint Alignment { get { return 4; } }
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
			entries.LayOutChildren();
		}

		private const uint MetadataHeaderSize = 4u;
	}
}
