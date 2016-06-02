using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile.Raw
{
	public struct Rva<T> : IFixedSizeObject
		where T : struct
	{
		public Rva(uint address)
		{
			this.Address = address;
		}

		public uint Address;

		public bool IsNull { get { return Address == 0; } }

		public T Read(/* TODO */)
		{
			throw new NotImplementedException();
		}

		public static readonly Rva<T> Null = new Rva<T>();
	}

	public struct RvaToArray<T> : IFixedSizeObject
		where T : struct
	{
		public RvaToArray(uint address)
		{
			this.Address = address;
		}

		public uint Address;

		public bool IsNull { get { return Address == 0; } }

		public StructArray<T> Read(/* TODO */)
		{
			throw new NotImplementedException();
		}

		public static readonly RvaToArray<T> Null = new RvaToArray<T>();
	}
}
