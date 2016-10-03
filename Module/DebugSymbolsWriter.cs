using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;
using Osprey.ModuleFile;
using Raw = Osprey.ModuleFile.Raw;

namespace Osprey
{
	/* 
	 * The debug symbols file format makes much heavier use of varibale-size structs than
	 * the module file format, which means there's also a greater use of RVAs, in the form
	 * of indirecton tables. The format is laid out as follows (notation explained below):
	 * 
	 *   DebugSymbolsHeader~
	 *     -> SourceFileList~
	 *     [-> MethodSymbols~]
	 *   SourceFileList
	 *     [-> SourceFile~]
	 *   [SourceFile~]
	 *   [
	 *     MethodSymbols~
	 *       [-> OverloadSymbols~]
	 *     [
	 *       OverloadSymbols~
	 *         [DebugSymbol]
	 *     ]
	 *   ]
	 *   
	 * where
	 * 
	 *   T~    =  T is variable-size
	 *   [T]   =  array of T
	 *   -> T  =  RVA to T
	 *   
	 * DebugSymbol is indeed the only fixed-size struct in this format.
	 */
	public class DebugSymbolsWriter
	{
		public DebugSymbolsWriter()
		{
			this.sourceFiles = new FileObjectArray<SourceFileObject>(null, 5);
			this.sourceFileToIndex = new Dictionary<SourceFile, int>(5);
			this.methodSymbols = new FileObjectArray<MethodSymbolsObject>(null, 50);
		}

		private FileObjectArray<SourceFileObject> sourceFiles;
		private Dictionary<SourceFile, int> sourceFileToIndex;
		private FileObjectArray<MethodSymbolsObject> methodSymbols;

		private uint GetSourceFileListAddress()
		{
			// SourceFileList is emitted right after the array of RVAs to MethodSymbols.
			var methodSymbolsIndirectionTableSize =
				unchecked(sizeof(uint) * (uint)methodSymbols.Count);
			return HeaderSize + methodSymbolsIndirectionTableSize;
		}

		public int GetSourceFileIndex(SourceFile file)
		{
			int index;
			if (!sourceFileToIndex.TryGetValue(file, out index))
			{
				index = sourceFiles.Count;
				var sourceFileObject = new SourceFileObject(file);
				sourceFileToIndex[file] = index;
				sourceFiles.Add(sourceFileObject);
			}
			return index;
		}

		public void AddMethodSymbols(Members.MethodGroup method)
		{
			var symbols = new MethodSymbolsObject(method, this);
			methodSymbols.Add(symbols);
		}

		public uint LayOutMembers()
		{
			var sourceFileListAddress = GetSourceFileListAddress();
			// The SourceFileList begins with an indirection table for all the source files.
			var sourceFileIndirectionTableSize =
				unchecked(sizeof(uint) * (uint)sourceFiles.Count);

			sourceFiles.RelativeAddress = unchecked(
				sourceFileListAddress +
				SourceFileListSize +
				sourceFileIndirectionTableSize
			);
			sourceFiles.LayOutChildren();

			methodSymbols.RelativeAddress = sourceFiles.Address + sourceFiles.AlignedSize;
			methodSymbols.LayOutChildren();

			return methodSymbols.RelativeAddress + methodSymbols.Size;
		}

		public void Emit(MemoryMappedFile file)
		{
			using (var view = file.CreateViewAccessor())
			{
				var header = new Raw.DebugSymbolsHeaderStruct();
				header.MagicNumber = MagicNumber;
				header.Metadata = 0u; // not used

				var sourceFileListAddress = GetSourceFileListAddress();
				header.SourceFiles = new Raw.Rva<Raw.SourceFileListStruct>(sourceFileListAddress);

				header.MethodSymbolCount = methodSymbols.Count;
				view.Write(0, ref header);

				EmitSourceFileList(view, sourceFileListAddress);
				EmitMethodSymbols(view, HeaderSize);
			}
		}

		private void EmitSourceFileList(MemoryMappedViewAccessor view, uint address)
		{
			var header = new Raw.SourceFileListStruct();
			header.FileCount = sourceFiles.Count;
			view.Write(address, ref header);

			address += SourceFileListSize;

			foreach (var sourceFile in sourceFiles)
			{
				view.Write(address, sourceFile.Address);
				address += sizeof(uint);

				sourceFile.Emit(view);
			}
		}

		private void EmitMethodSymbols(MemoryMappedViewAccessor view, uint tableAddress)
		{
			foreach (var methodSymbol in methodSymbols)
			{
				view.Write(tableAddress, methodSymbol.Address);
				tableAddress += sizeof(uint);

				methodSymbol.Emit(view);
			}
		}

		private const uint MagicNumber =
			(79u)       | // O
			(86u << 8)  | // V
			(68u << 16) | // D
			(83u << 24);  // S
		private const uint HeaderSize = 16u;
		private const uint SourceFileListSize = 4u;
	}
}
