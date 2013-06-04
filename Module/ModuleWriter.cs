using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Osprey
{
	public class ModuleWriter : BinaryWriter
	{
		public ModuleWriter(Stream output)
			: base(output)
		{ }
		public ModuleWriter(Stream output, Encoding encoding)
			: base(output, encoding)
		{ }

		private Stack<long> collections = new Stack<long>();

		public void BeginCollection(int length)
		{
			collections.Push(OutStream.Position); // Save the current position, which will contain the size
			Seek(4, SeekOrigin.Current); // Skip ahead 4 bytes
			Write(length); // Write the length

			// Let the user write the contents
		}

		public void EndCollection()
		{
			var sizeOffset = collections.Pop(); // Recover the position of the size
			var currentOffset = OutStream.Position;

			var size = currentOffset - sizeOffset - 4;
			OutStream.Seek(sizeOffset, SeekOrigin.Begin); // Go back to where the size needs to be
			Write(checked((uint)size)); // Write the size, yay!
			OutStream.Seek(currentOffset, SeekOrigin.Begin); // Continue at the correct offset
		}

		public void Write(Version value)
		{
			if (value == null)
				throw new ArgumentNullException("value");

			Write(value.Major);
			Write(value.Minor);
			Write(value.Build);
			Write(value.Revision);
		}

		public override void Write(string value)
		{
			if (value == null)
				throw new ArgumentNullException("value");

			Write(value.Length);
			for (var i = 0; i < value.Length; i++)
				Write(unchecked((ushort)value[i]));
		}

		public void WriteCString(string value)
		{
			if (value == null)
				throw new ArgumentNullException("value");

			Write(value.Length + 1); // Characters + zero terminator
			for (var i = 0; i < value.Length; i++)
				Write(checked((sbyte)value[i]));
			Write((byte)0); // Zero terminator, must be included
		}

		public void WriteFlags(Module.TypeFlags value)
		{
			Write((uint)value);
		}

		public void WriteFlags(Module.FieldFlags value)
		{
			Write((uint)value);
		}

		public void WriteFlags(Module.MethodFlags value)
		{
			Write((uint)value);
		}

		public void WriteFlags(Module.OverloadFlags value)
		{
			Write((uint)value);
		}

		public void WriteFlags(Module.ConstantFlags value)
		{
			Write((uint)value);
		}

		public void WriteFlags(Module.Operator value)
		{
			Write((byte)value);
		}
	}
}