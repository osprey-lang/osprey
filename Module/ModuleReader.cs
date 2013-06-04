using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Reads module data from a <see cref="FileStream"/>.
	/// </summary>
	internal class ModuleReader : BinaryReader
	{
		public ModuleReader(FileStream input)
			: base(input)
		{
			fileName = input.Name;
		}

		public ModuleReader(FileStream input, Encoding encoding)
			: base(input, encoding)
		{
			fileName = input.Name;
		}

		private string fileName;
		/// <summary>
		/// Gets the name of the file from which the module is being loaded.
		/// </summary>
		public string FileName { get { return fileName; } }

		public string ReadOvumString()
		{
			var length = ReadInt32();
			var data = ReadChars(length);

			return new string(data);
		}

		public Version ReadVersion()
		{
			var major = ReadInt32();
			var minor = ReadInt32();
			var build = ReadInt32();
			var revision = ReadInt32();

			return new Version(major, minor, build, revision);
		}

		public Module.TypeFlags ReadTypeFlags()
		{
			return (Module.TypeFlags)ReadUInt32();
		}

		public Module.FieldFlags ReadFieldFlags()
		{
			return (Module.FieldFlags)ReadUInt32();
		}

		public Module.MethodFlags ReadMethodFlags()
		{
			return (Module.MethodFlags)ReadUInt32();
		}

		public Module.OverloadFlags ReadOverloadFlags()
		{
			return (Module.OverloadFlags)ReadUInt32();
		}

		public Module.ConstantFlags ReadConstantFlags()
		{
			return (Module.ConstantFlags)ReadUInt32();
		}

		public Module.Operator ReadOperator()
		{
			return (Module.Operator)ReadByte();
		}

		public void SkipHeader()
		{
			var magicNumber = ReadBytes(4);
			for (var i = 0; i < 4; i++)
				if (magicNumber[i] != Module.MagicNumber[i])
					throw new ModuleLoadException(fileName, "Invalid magic number in file.");

			Seek(Module.DataStart, SeekOrigin.Begin);
		}

		/// <summary>
		/// Skips past a size-prefixed collection.
		/// </summary>
		public void SkipCollection()
		{
			var size = ReadUInt32();
			Seek(size, SeekOrigin.Current);
		}

		public long Seek(long offset, SeekOrigin origin)
		{
			return BaseStream.Seek(offset, origin);
		}
	}
}