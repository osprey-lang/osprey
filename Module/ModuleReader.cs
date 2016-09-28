using System;
using System.Collections.Generic;
using System.IO;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;
using Raw = Osprey.ModuleFile.Raw;

namespace Osprey
{
	/// <summary>
	/// Reads module data from a <see cref="FileStream"/>.
	/// </summary>
	internal class ModuleReader : IDisposable
	{
		public ModuleReader(string fileName)
		{
			if (fileName == null)
				throw new ArgumentNullException("fileName");

			this.file = MemoryMappedFile.CreateFromFile(fileName, FileMode.Open, null, 0, MemoryMappedFileAccess.Read);
			this.fileName = fileName;
			this.View = file.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read);
		}

		private MemoryMappedFile file;

		/// <summary>
		/// A view into the memory-mapped file, over the entire file.
		/// </summary>
		public readonly MemoryMappedViewAccessor View;

		private string fileName;
		/// <summary>
		/// Gets the name of the file from which the module is being loaded.
		/// </summary>
		public string FileName { get { return fileName; } }

		public void Dispose()
		{
			Dispose(true);
		}

		private void Dispose(bool disposing)
		{
			if (disposing)
			{
				View.Dispose();
				file.Dispose();
			}
		}

		public int ReadInt32(uint address)
		{
			return View.ReadInt32(address);
		}

		public uint ReadUInt32(uint address)
		{
			return View.ReadUInt32(address);
		}

		public string ReadString(uint address)
		{
			// Ovum modules and .NET are both UTF-16
			var length = View.ReadInt32(address);
			if (length == 0)
				return "";

			var chars = ReadArray<char>(address + Raw.StringStruct.CharactersOffset, length);
			return new string(chars);
		}

		public string ReadString(Raw.Rva<Raw.StringStruct> rva)
		{
			return ReadString(rva.Address);
		}

		public T Read<T>(uint address)
			where T : struct
		{
			T result;
			View.Read(address, out result);
			return result;
		}

		public void Read<T>(uint address, out T result)
			where T : struct
		{
			View.Read(address, out result);
		}

		public T Deref<T>(Raw.Rva<T> rva)
			where T : struct
		{
			T result;
			View.Read(rva.Address, out result);
			return result;
		}

		public void Deref<T>(Raw.Rva<T> rva, out T result)
			where T : struct
		{
			View.Read(rva.Address, out result);
		}

		public T[] ReadArray<T>(uint address, int count)
			where T : struct
		{
			var result = new T[count];
			ReadArray(address, count, result);
			return result;
		}

		public void ReadArray<T>(uint address, int count, T[] result)
			where T : struct
		{
			var itemsRead = View.ReadArray(address, result, 0, count);
			if (itemsRead != count)
				throw new ModuleLoadException(
					fileName,
					string.Format("Wrong number of array items read ({0}, expected {1})", count, itemsRead)
				);
		}
	}
}