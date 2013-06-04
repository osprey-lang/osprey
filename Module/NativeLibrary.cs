using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Loads a native Windows DLL (not a managed DLL) and provides access to the DLL's procedures.
	/// This is used by the compiler to verify that linked DLLs actually exports the methods that
	/// native-code modules rely on.
	/// </summary>
	internal class NativeLibrary : IDisposable
	{
		private NativeLibrary(string fileName, IntPtr handle)
		{
			this.fileName = fileName;
			this.handle = handle;
		}

		~NativeLibrary()
		{
			Dispose();
		}

		private IntPtr handle;

		private string fileName;
		/// <summary>
		/// Gets the name of the file that the library was loaded from.
		/// </summary>
		public string FileName { get { return fileName; } }

		/// <summary>
		/// Disposes of the native library's unmanaged resources.
		/// </summary>
		public void Dispose()
		{
			if (handle != IntPtr.Zero)
			{
				FreeLibrary(handle);
				handle = IntPtr.Zero;
			}
		}

		public bool ContainsMethod(string name)
		{
			return GetProcAddress(handle, name) != IntPtr.Zero;
		}

		/// <summary>
		/// Opens a native library from the specified file.
		/// </summary>
		/// <param name="fileName">The name of the file to open.</param>
		/// <returns>A new <see cref="NativeLibrary"/> instance for the specified library.</returns>
		/// <remarks>
		/// This implementation does NOT rely on LoadLibrary's default DLL loading mechanism.
		/// Instead, if the fileName is relative, this implementation resolve it relative to the current
		/// working directory. System folders which usually contain libraries are NOT examined.
		/// </remarks>
		public static NativeLibrary Open(string fileName)
		{
			fileName = Path.GetFullPath(fileName);
			if (!File.Exists(fileName))
				throw new FileNotFoundException("The specified library file could not be found.", fileName);

			// Note: we pass the DONT_RESOLVE_DLL_REFERENCES flag to avoid loading dependent DLLs.
			// We don't actually want to load them, or run DllMain or anything, because we're only
			// interested in checking the existence of methods within the DLL.
			// Unfortunately, DONT_RESOLVE_DLL_REFERENCES is deprecated, so it MAY be removed from
			// a Windows version in the future, but there's no other easy way of doing this, short
			// of implementing our own code for reading from DLLs, and frankly, do not want.
			var handle = LoadLibraryEx(fileName, IntPtr.Zero, DONT_RESOLVE_DLL_REFERENCES);

			if (handle == IntPtr.Zero)
				// An error occurred!
				throw new Exception("Could not load native library from " + fileName);

			return new NativeLibrary(fileName, handle);
		}

		public override int GetHashCode()
		{
			return handle.GetHashCode();
		}

		public override bool Equals(object obj)
		{
			if (obj is NativeLibrary)
				return this == (NativeLibrary)obj;

			return base.Equals(obj);
		}

		public bool Equals(NativeLibrary other)
		{
			return this == other;
		}

		public static bool operator ==(NativeLibrary left, NativeLibrary right)
		{
			if ((object)left == null || (object)right == null)
				return (object)left == (object)right; // either is null

			return left.handle == right.handle;
		}
		public static bool operator !=(NativeLibrary left, NativeLibrary right)
		{
			if ((object)left == null || (object)right == null)
				return (object)left != (object)right; // either is null

			return left.handle != right.handle;
		}

		private const int DONT_RESOLVE_DLL_REFERENCES = 0x00000001;

		[DllImport("kernel32.dll")]
		private static extern IntPtr LoadLibraryEx(string lpFileName, IntPtr hFile, int dwFlags);
		[DllImport("kernel32.dll")]
		private static extern IntPtr GetProcAddress(IntPtr hModule, string lpProcName);
		[DllImport("kernel32.dll")]
		private static extern bool FreeLibrary(IntPtr hModule);
	}
}