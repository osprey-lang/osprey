using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Osprey.ModuleFile.Raw
{
	[StructLayout(LayoutKind.Sequential)]
	public struct StructArray<T>
		where T : struct
	{
		public uint Offset;
		public T[] Values;
	}
}
