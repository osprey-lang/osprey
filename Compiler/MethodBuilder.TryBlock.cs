using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Instructions;
using Type = Osprey.Members.Type;

namespace Osprey
{
	public abstract class TryBlockMember
	{
		protected TryBlockMember(MethodBuilder method, TryBlock parent)
		{
			this.method = method;
			this.parent = parent;
		}

		private MethodBuilder method;
		/// <summary>
		/// Gets the 
		/// </summary>
		public MethodBuilder Method { get { return method; } }

		private TryBlock parent;
		/// <summary>
		/// Gets the try block that this member belongs to, or null if it does not belong to one.
		/// </summary>
		public TryBlock Parent { get { return parent; } }

		private Instruction start, end;
		private int startOffset, endOffset;
		/// <summary>
		/// Gets the first instruction of the member.
		/// </summary>
		public Instruction Start { get { return start; } }
		/// <summary>
		/// Gets the byte offset of the start instruction.
		/// </summary>
		public int StartOffset { get { return start == null ? startOffset : start.ByteOffset; } }

		/// <summary>
		/// Gets the last instruction of the member.
		/// </summary>
		public Instruction End { get { return end; } }
		/// <summary>
		/// Gets the byte offset of the end instruction, plus its size.
		/// </summary>
		public int EndOffset { get { return end == null ? endOffset : end.ByteOffset + end.GetSize(); } }

		internal List<TryBlock> childTryBlocks;

		public bool HasChildTryBlocks { get { return childTryBlocks != null && childTryBlocks.Count > 0; } }

		public TryBlock[] GetChildTryBlocks()
		{
			if (childTryBlocks == null)
				return new TryBlock[0];
			return childTryBlocks.ToArray();
		}

		protected internal void AddChildTryBlock(TryBlock child)
		{
			if (child == null)
				throw new ArgumentNullException("child");

			if (childTryBlocks == null)
				childTryBlocks = new List<TryBlock>();

			childTryBlocks.Add(child);
		}

		public void BeginBlock()
		{
			if (start != null)
				throw new InvalidOperationException("The block has already been started.");

			method.AddNewTryMember(this);
		}

		internal void BeginBlock(Instruction startInstr)
		{
			if (startInstr == null)
				throw new ArgumentNullException("startInstr");
			this.start = startInstr;
		}

		public void EndBlock()
		{
			if (end != null)
				throw new InvalidOperationException("The block has already been ended.");

			end = method.LastInstruction;
		}

		/// <summary>
		/// Prevents future modifications of the try block member, and finalizes its offsets.
		/// This should only be called after the method has been fully constructed; anything
		/// else results in undefined behaviour.
		/// </summary>
		internal void Lock()
		{
			method = null;
			startOffset = start.ByteOffset;
			endOffset = end.ByteOffset + end.GetSize();

			start = end = null;
			method = null;

			if (HasChildTryBlocks)
				for (var i = 0; i < childTryBlocks.Count; i++)
					childTryBlocks[i].Lock();

			LockInternal();
		}

		protected virtual void LockInternal() { }
	}

	public sealed class TryBlock : TryBlockMember
	{
		public TryBlock(MethodBuilder method, TryBlock parent)
			: base(method, parent)
		{ }

		private TryBlockKind kind;
		/// <summary>
		/// Gets the kind of the try block.
		/// </summary>
		public TryBlockKind Kind { get { return kind; } }

		private List<CatchBlock> catches;
		internal List<CatchBlock> Catches { get { return catches; } }

		private FinallyBlock _finally;
		/// <summary>
		/// Gets the finally block associated with the try block.
		/// </summary>
		public FinallyBlock Finally { get { return _finally; } }

		public bool IsFinished
		{
			get
			{
				if (kind == TryBlockKind.TryCatch)
					return catches[catches.Count - 1].End != null;
				if (kind == TryBlockKind.TryFinally)
					return _finally.End != null;
				return false; // nope
			}
		}

		/// <summary>
		/// Returns an array of the catches belonging to the try block.
		/// </summary>
		/// <returns>An array of the catches belonging to the try block.</returns>
		public CatchBlock[] GetCatches()
		{
			if (catches == null)
				return new CatchBlock[0];
			return catches.ToArray();
		}

		public CatchBlock AddCatch(uint catchTypeId)
		{
			if (kind == TryBlockKind.TryFinally)
				throw new InvalidOperationException("Cannot add a catch clause to a try-finally.");

			if (catches == null)
			{
				catches = new List<CatchBlock>();
				kind = TryBlockKind.TryCatch;
			}
			else if (catches[catches.Count - 1].End == null)
				throw new InvalidOperationException("Cannot add a new catch clause until the last catch has been ended.");

			var @catch = new CatchBlock(this, catchTypeId);
			catches.Add(@catch);
			@catch.BeginBlock();

			return @catch;
		}

		public FinallyBlock AddFinally()
		{
			if (kind == TryBlockKind.TryCatch)
				throw new InvalidOperationException("Cannot add a finally block to a try-catch.");

			if (_finally != null)
				throw new InvalidOperationException("The try block already has a finally block.");

			kind = TryBlockKind.TryFinally;
			_finally = new FinallyBlock(this);
			_finally.BeginBlock();
			return _finally;
		}

		protected override void LockInternal()
		{
			if (kind == TryBlockKind.TryCatch)
				foreach (var c in catches)
					c.Lock();
			else if (kind == TryBlockKind.TryFinally)
				_finally.Lock();
		}
	}

	public sealed class CatchBlock : TryBlockMember
	{
		public CatchBlock(TryBlock parent, uint catchTypeId)
			: base(parent.Method, parent)
		{
			this.typeId = catchTypeId;
		}

		private uint typeId;
		/// <summary>
		/// Gets the ID of the type captured by this catch block.
		/// </summary>
		public uint TypeId { get { return typeId; } }
	}

	public sealed class FinallyBlock : TryBlockMember
	{
		public FinallyBlock(TryBlock parent)
			: base(parent.Method, parent)
		{ }
	}

	public enum TryBlockKind
	{
		/// <summary>
		/// The try block has not had a finally or catch added to it yet; it is therefore
		/// not a valid try block.
		/// </summary>
		Invalid = 0,
		/// <summary>
		/// The try block is a try-catch block, with one or more catches and no finally.
		/// </summary>
		TryCatch = 1,
		/// <summary>
		/// The try block is a try-finally block, with exactly one finally and no catches.
		/// </summary>
		TryFinally = 2,
	}
}