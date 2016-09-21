using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	// Note: For performance reasons, there is no DebugSymbolObject class. That data
	// is already contained in CompiledMethodData.DebugSymbols, and since there is
	// probably a large number of function members with a large total number of debug
	// symbols, we'd prefer not to duplicate all of it. It's a waste of CPU time and
	// memory.
	// Unfortunately we do need MethodSymbolsObject and OverloadSymbolsObject classes
	// for layout purposes, as these objects are variable-size and are referred to
	// with RVAs.

	public class SourceFileObject : FileObject
	{
		public SourceFileObject(SourceFile file)
		{
			this.Hash = file.FileHash;
			this.FileName = new WideString(file.FileName);
			this.FileName.LayoutParent = this;
		}

		public readonly byte[] Hash;
		public readonly WideString FileName;

		public override uint Size
		{
			get
			{
				return unchecked((uint)Hash.Length + FileName.Size);
			}
		}

		public override uint Alignment { get { return 4; } }

		public override void LayOutChildren()
		{
			FileName.RelativeAddress = unchecked((uint)Hash.Length);
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			view.WriteArray(this.Address, Hash, 0, Hash.Length);
			FileName.Emit(view);
		}
	}

	public class MethodSymbolsObject : FileObject
	{
		public MethodSymbolsObject(Members.MethodGroup method, DebugSymbolsWriter writer)
		{
			this.Method = method;
			overloads = new FileObjectArray<OverloadSymbolsObject>(this, method.Count);
			AddOverloads(method, writer);
		}

		public readonly Members.MethodGroup Method;
		private FileObjectArray<OverloadSymbolsObject> overloads;

		public override uint Size
		{
			get
			{
				// BaseSize + size of RVA array + size of overload symbols
				return unchecked(
					BaseSize +
					(uint)(4 * overloads.Count) +
					overloads.AlignedSize
				);
			}
		}

		public override uint Alignment { get { return 4; } }

		private void AddOverloads(IEnumerable<Members.Method> overloads, DebugSymbolsWriter writer)
		{
			foreach (var overload in overloads)
			{
				if (overload.CompiledMethod != null &&
					overload.CompiledMethod.DebugSymbols != null &&
					overload.CompiledMethod.DebugSymbols.Length > 0)
					this.overloads.Add(new OverloadSymbolsObject(overload, writer));
				else
					this.overloads.Add(OverloadSymbolsObject.Empty);
			}
		}

		public override void LayOutChildren()
		{
			// The MethodSymbols struct contains an array of RVAs. The actual
			// OverloadSymbols values have to be put /after/ that.
			var rvaArraySize = unchecked((uint)(4 * overloads.Count));
			overloads.RelativeAddress = BaseSize + rvaArraySize;
			overloads.LayOutChildren();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var address = this.Address;

			var data = new Raw.MethodSymbolsStruct();
			data.MemberToken = new MetadataToken(Method.Id);
			data.Metadata = 0u; // not used
			data.OverloadCount = overloads.Count;
			view.Write(address, ref data);

			address += BaseSize;
			// Array of RVAs to each overload's symbols follows
			foreach (var overload in overloads)
			{
				view.Write(address, overload.Address);
				address += sizeof(uint);

				if (!overload.IsEmpty)
					overload.Emit(view);
			}
		}

		// memberToken + metadata + overloadCount
		private const uint BaseSize = 12;
	}

	public class OverloadSymbolsObject : FileObject
	{
		// Special constructor used only for a "null" OverloadsSymbolObject.
		private OverloadSymbolsObject()
		{
			this.debugWriter = null;
			this.DebugSymbols = null;
		}
		public OverloadSymbolsObject(Members.Method overload, DebugSymbolsWriter writer)
		{
			this.debugWriter = writer;
			this.DebugSymbols = overload.CompiledMethod.DebugSymbols;

			// It is currently not possible for an overload to have
			// debug symbols from more than one source file. So we
			// just have to make sure that one source file is in the
			// writer, for when it's time to emit symbols. This is
			// terribly ugly, yes.
			writer.GetSourceFileIndex(this.DebugSymbols[0].File);
		}

		private readonly DebugSymbolsWriter debugWriter;
		public readonly SourceLocation[] DebugSymbols;

		public bool IsEmpty { get { return DebugSymbols == null; } }

		public override uint Size
		{
			get
			{
				if (IsEmpty)
					return 0u;
				else
					// BaseSize + size of debug symbols array
					return unchecked(
						BaseSize +
						(uint)(DebugSymbolSize * DebugSymbols.Length)
					);
			}
		}

		public override uint Alignment { get { return 4; } }

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var address = this.Address;

			var data = new Raw.OverloadSymbolsStruct();
			data.Metadata = 0u; // not used
			data.SymbolCount = DebugSymbols.Length;
			view.Write(address, ref data);
			address += BaseSize;

			foreach (var symbol in DebugSymbols)
			{
				var symbolData = new Raw.DebugSymbolStruct();
				symbolData.StartOffset = unchecked((uint)symbol.BytecodeStartOffset);
				symbolData.EndOffset = unchecked((uint)symbol.BytecodeEndOffset);
				symbolData.SourceFile = debugWriter.GetSourceFileIndex(symbol.File);
				symbolData.StartLocation = GetSourceLocation(symbol.File, symbol.SourceStartIndex);
				view.Write(address, ref symbolData);
				address += DebugSymbolSize;
			}
		}

		public new Raw.Rva<T> ToRva<T>()
			where T : struct
		{
			if (IsEmpty)
				return Raw.Rva<T>.Null;
			else
				return new Raw.Rva<T>(this.Address);
		}

		// metadata + symbolCount
		private const uint BaseSize = 8;
		private const uint DebugSymbolSize = 28;

		public static readonly OverloadSymbolsObject Empty = new OverloadSymbolsObject();

		private static Raw.SourceLocationStruct GetSourceLocation(SourceFile file, int sourceIndex)
		{
			int column;
			int line = file.GetLineNumber(sourceIndex, 1, out column);
			return new Raw.SourceLocationStruct
			{
				LineNumber = line,
				Column = column,
			};
		}
	}
}
