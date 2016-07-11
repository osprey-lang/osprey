using System;
using System.Collections.Generic;
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

	public class MethodSymbolsObject : FileObject
	{
		public MethodSymbolsObject(Members.MethodGroup method)
		{
			this.Method = method;
			overloads = new FileObjectArray<OverloadSymbolsObject>(this, method.Count);
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

		private void AddOverloads(IEnumerable<Members.Method> overloads)
		{
			foreach (var overload in overloads)
			{
				this.overloads.Add(new OverloadSymbolsObject(overload));
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

		// memberToken + metadata + overloadCount
		private const uint BaseSize = 12;
	}

	public class OverloadSymbolsObject : FileObject
	{
		public OverloadSymbolsObject(Members.Method overload)
		{
			this.Overload = overload;
			this.debugSymbols = overload.CompiledMethod.DebugSymbols;
		}

		public readonly Members.Method Overload;
		private readonly SourceLocation[] debugSymbols;

		public override uint Size
		{
			get
			{
				// BaseSize + size of debug symbols array
				return unchecked(
					BaseSize +
					(uint)(DebugSymbolSize * Overload.CompiledMethod.DebugSymbols.Length)
				);
			}
		}

		public override uint Alignment { get { return 4; } }

		// metadata + symbolCount
		private const uint BaseSize = 8;
		private const uint DebugSymbolSize = 28;
	}
}
