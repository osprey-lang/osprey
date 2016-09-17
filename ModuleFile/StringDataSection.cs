using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class WideString : FileObject
	{
		public WideString(string value)
		{
			this.Value = value;
		}

		public readonly string Value;

		public override uint Size
		{
			get
			{
				// 4 for the length, plus 2 per System.Char
				// Ovum and .NET are both UTF-16.
				return unchecked(4u + 2 * (uint)Value.Length);
			}
		}

		public override uint Alignment { get { return 4; } }
	}

	public class ByteString : FileObject
	{
		public ByteString(string value)
		{
			this.Value = value;
			this.utf8Length = unchecked((uint)Encoding.UTF8.GetByteCount(value));
		}

		public readonly string Value;

		private uint utf8Length;

		public override uint Size
		{
			get
			{
				// 4 bytes for the string length, plus 1 byte per character,
				// encoded as UTF-8
				return 4u + utf8Length;
			}
		}

		public override uint Alignment { get { return 4; } }
	}

	public class StringDataSection : FileSection
	{
		public StringDataSection()
		{
			strings = new FileObjectArray<WideString>(this, 50);
			byteStrings = new FileObjectArray<ByteString>(this, 0);
		}

		public override uint Size
		{
			get
			{
				// No length needed for the strings or byteStrings arrays
				return RvaArraySize + strings.AlignedSize + byteStrings.AlignedSize;
			}
		}

		public override uint Alignment { get { return 4; } }

		// The string data section begins with an array of string RVAs.
		public uint RvaArraySize
		{
			// +4 for the length
			get { return unchecked(4 + 4 * (uint)strings.Count); }
		}

		// The Module takes care of wide string pooling, but we still need a
		// mapping so that we can obtain addresses of string values.
		private FileObjectArray<WideString> strings;
		private Dictionary<string, WideString> stringMapping = new Dictionary<string, WideString>(StringComparer.Ordinal);

		// For byte strings, we must do the pooling. Note that byte strings have
		// no metadata tokens, only addresses.
		private FileObjectArray<ByteString> byteStrings;
		private Dictionary<string, ByteString> byteStringMapping = new Dictionary<string, ByteString>(StringComparer.Ordinal);

		public WideString GetWideString(string value)
		{
			return stringMapping[value];
		}

		public WideString AddString(string value)
		{
			WideString ws;

			if (!stringMapping.TryGetValue(value, out ws))
			{
				ws = new WideString(value);
				strings.Add(ws);
				stringMapping.Add(value, ws);
			}

			return ws;
		}

		public ByteString GetByteString(string value)
		{
			return byteStringMapping[value];
		}

		public ByteString AddByteString(string value)
		{
			ByteString bs;

			if (!byteStringMapping.TryGetValue(value, out bs))
			{
				bs = new ByteString(value);
				byteStrings.Add(bs);
				byteStringMapping.Add(value, bs);
			}

			return bs;
		}

		public override void LayOutChildren()
		{
			strings.RelativeAddress = RvaArraySize;
			strings.LayOutChildren();

			byteStrings.RelativeAddress = strings.RelativeAddress + strings.AlignedSize;
			byteStrings.LayOutChildren();
		}
	}
}
