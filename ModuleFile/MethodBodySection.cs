using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public abstract class MethodBody : FileObject
	{
		public override uint Alignment { get { return 8; } }

		public abstract override int GetHashCode();

		public abstract override bool Equals(object obj);

		public abstract override void LayOutChildren();

		protected static int GetBodyHashCode(byte[] body)
		{
			var hash = 0;

			var i = 0;
			var length = body.Length - 4;
			while (i < length)
			{
				unchecked
				{
					hash ^= body[i];
					hash ^= 7 * body[i + 1] << 8;
					hash ^= 13 * body[i + 2] << 16;
					hash ^= 23 * body[i + 3] << 24;
				}
				i += 4;
			}

			while (i < body.Length)
			{
				unchecked
				{
					hash ^= body[i] << (3 * (i & 3));
				}
				i++;
			}

			return hash;
		}

		protected static bool BodyEquals(byte[] a, byte[] b)
		{
			if (a == b)
				return true;
			if (a == null || b == null)
				return false;
			if (a.Length != b.Length)
				return false;

			var iMax = a.Length;
			for (var i = 0; i < iMax; i++)
				if (a[i] != b[i])
					return false;

			return true;
		}
	}

	public class ShortMethodBody : MethodBody
	{
		public ShortMethodBody(Members.Method method)
		{
			this.body = method.CompiledMethod.BodyBytes;
		}

		private byte[] body;

		private int? hashCode;

		public override uint Size { get { return unchecked(4 + (uint)body.Length); } }

		public override int GetHashCode()
		{
			if (hashCode == null)
				hashCode = GetBodyHashCode(body);
			return hashCode.Value;
		}

		public override bool Equals(object obj)
		{
			var other = obj as ShortMethodBody;
			if (other != null)
				return Equals(other);
			return false;
		}

		public bool Equals(ShortMethodBody other)
		{
			return BodyEquals(this.body, other.body);
		}

		public override void LayOutChildren()
		{
			// Nothing to be done here
		}
	}

	public abstract class TryBlockObject : FileObject
	{
		public TryBlockObject(TryBlock tryBlock)
		{
			this.TryBlock = tryBlock;
		}

		public readonly TryBlock TryBlock;

		public override uint Size { get { return 20; } }

		public override uint Alignment { get { return _Alignment; } }

		internal const uint _Alignment = 4;
	}

	public class TryFinallyBlock : TryBlockObject
	{
		public TryFinallyBlock(TryBlock tryBlock)
			: base(tryBlock)
		{
			this.FinallyBlock = tryBlock.Finally;
		}

		public readonly FinallyBlock FinallyBlock;
	}

	public class TryCatchBlock : TryBlockObject
	{
		public TryCatchBlock(TryBlock tryBlock, CatchClauseObject[] catchClauses)
			: base(tryBlock)
		{
			this.CatchClauses = catchClauses;
		}

		public readonly CatchClauseObject[] CatchClauses;
	}

	public class CatchClauseObject : FileObject
	{
		public CatchClauseObject(CatchBlock catchBlock)
		{
			this.CatchBlock = catchBlock;
		}

		public readonly CatchBlock CatchBlock;

		public override uint Size { get { return 12; } }

		public override uint Alignment { get { return 4; } }

		internal const uint _Alignment = 4;
	}

	public class LongMethodBody : MethodBody
	{
		public LongMethodBody(Members.Method method)
		{
			var compiledMethod = method.CompiledMethod;
			this.localCount = compiledMethod.LocalCount;
			this.maxStack = compiledMethod.MaxStack;
			this.body = compiledMethod.BodyBytes;

			if (compiledMethod.TryBlocks == null || compiledMethod.TryBlocks.Length == 0)
			{
				this.tryBlocks = new FileObjectArray<TryBlockObject>(this);
				this.catchBlocks = new FileObjectArray<CatchClauseObject>(this);
			}
			else
			{
				this.tryBlocks = new FileObjectArray<TryBlockObject>(this, compiledMethod.TryBlocks.Length);
				this.catchBlocks = new FileObjectArray<CatchClauseObject>(this, compiledMethod.TryBlocks.Length);
				AddTryBlocks(compiledMethod.TryBlocks);
			}
		}

		private int localCount;
		private int maxStack;
		private byte[] body;
		private FileObjectArray<TryBlockObject> tryBlocks;
		private FileObjectArray<CatchClauseObject> catchBlocks;

		private int? hashCode;

		public override uint Size
		{
			get
			{
				var size = BaseSize + unchecked((uint)body.Length);
				if (tryBlocks.Count > 0)
				{
					size = AlignTo(size, TryBlockObject._Alignment);
					size += tryBlocks.AlignedSize;
					size = AlignTo(size, CatchClauseObject._Alignment);
					size += catchBlocks.AlignedSize;
				}
				return size;
			}
		}

		private void AddTryBlocks(IEnumerable<TryBlock> tryBlocks)
		{
			foreach (var tryBlock in tryBlocks)
			{
				if (tryBlock.HasChildTryBlocks)
					AddTryBlocks(tryBlock.childTryBlocks);

				if (tryBlock.Kind == TryBlockKind.TryFinally)
					AddTryFinallyBlock(tryBlock);
				else
					AddTryCatchBlock(tryBlock);
			}
		}

		private void AddTryFinallyBlock(TryBlock tryBlock)
		{
			tryBlocks.Add(new TryFinallyBlock(tryBlock));
		}

		private void AddTryCatchBlock(TryBlock tryBlock)
		{
			var catchClauses = tryBlock.Catches.Select(c => new CatchClauseObject(c)).ToArray();
			tryBlocks.Add(new TryCatchBlock(tryBlock, catchClauses));

			foreach (var catchClause in catchClauses)
			{
				this.catchBlocks.Add(catchClause);
				if (catchClause.CatchBlock.HasChildTryBlocks)
					AddTryBlocks(catchClause.CatchBlock.childTryBlocks);
			}
		}

		public override int GetHashCode()
		{
			if (hashCode == null)
			{
				// We don't hash the contents of try blocks, because we don't
				// compare them. But let's include the number of try blocks in
				// the hash code for (hopefully) better distribution.
				var metaHash = localCount ^ (maxStack << 8) ^ (tryBlocks.Count << 13);
				var bodyHash = GetBodyHashCode(body);
				hashCode = metaHash ^ bodyHash;
			}
			return hashCode.Value;
		}

		public override bool Equals(object obj)
		{
			var other = obj as LongMethodBody;
			if (other != null)
				return Equals(other);
			return false;
		}

		public bool Equals(LongMethodBody other)
		{
			if (object.ReferenceEquals(this, other))
				return true;

			// Don't compare try blocks
			if (this.tryBlocks.Count > 0 || other.tryBlocks.Count > 0)
				return false;

			if (this.localCount != other.localCount ||
				this.maxStack != other.maxStack)
				return false;

			return BodyEquals(this.body, other.body);
		}

		public override void LayOutChildren()
		{
			if (tryBlocks.Count > 0)
			{
				tryBlocks.RelativeAddress = AlignTo(BaseSize + unchecked((uint)body.Length), TryBlockObject._Alignment);
				tryBlocks.LayOutChildren();

				catchBlocks.RelativeAddress = AlignTo(tryBlocks.RelativeAddress + tryBlocks.AlignedSize, CatchClauseObject._Alignment);
				catchBlocks.LayOutChildren();
			}
		}

		// Base struct including TryBlocks RVA and length of method body
		private const uint BaseSize = 20;
	}

	public class NativeMethodBody : MethodBody
	{
		public NativeMethodBody(int localCount, ByteString entryPointName)
		{
			this.localCount = localCount;
			this.entryPointName = entryPointName;
			entryPointName.LayoutParent = this;
		}

		private int localCount;
		private ByteString entryPointName;

		public override uint Size
		{
			get { return 4 + entryPointName.Size; }
		}

		public override int GetHashCode()
		{
			return localCount ^ entryPointName.Value.GetHashCode();
		}

		public override bool Equals(object obj)
		{
			var other = obj as NativeMethodBody;
			if (other != null)
				return Equals(other);
			return false;
		}

		public bool Equals(NativeMethodBody other)
		{
			return this.localCount == other.localCount &&
				this.entryPointName.Value == other.entryPointName.Value;
		}

		public override void LayOutChildren()
		{
			entryPointName.RelativeAddress = 4;
		}
	}

	public class MethodBodySection : FileSection
	{
		public MethodBodySection()
		{
			methodBodyArray = new FileObjectArray<MethodBody>(this, 50);
			methodBodyDict = new Dictionary<MethodBody, MethodBody>();
		}

		private readonly FileObjectArray<MethodBody> methodBodyArray;
		// HashSet<MethodBody> would not let us obtain the actual value stored in the set, so
		// we have to use a Dictionary<,> here.
		private readonly Dictionary<MethodBody, MethodBody> methodBodyDict;

		public override uint Size { get { return methodBodyArray.Size; } }

		public override uint Alignment { get { return methodBodyArray.Alignment; } }

		public MethodBody Add(MethodBody item)
		{
			// If there is already an identical method body, reuse it. This will happen with
			// some kinds of auto-generated methods, such as parameterless constructors that
			// just call through to Object.'.new'().
			MethodBody existingItem;
			if (methodBodyDict.TryGetValue(item, out existingItem))
				return existingItem;

			methodBodyDict.Add(item, item);
			return item;
		}

		public override void LayOutChildren()
		{
			methodBodyArray.LayOutChildren();
		}
	}
}
