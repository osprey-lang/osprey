using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Instructions;

namespace Osprey
{
	public class MethodBuilder
	{
		public MethodBuilder(bool hasInstance, Nodes.Parameter[] parameters, Module module)
		{
			if (module == null)
				throw new ArgumentNullException("module");
			this.module = module;

			if (hasInstance)
				this.parameters.Add(new LocalVariable(0, null, false, true)); // 'this' argument

			if (parameters != null)
			{
				var paramStart = hasInstance ? 1 : 0;
				for (var i = 0; i < parameters.Length; i++)
					this.parameters.Add(new LocalVariable(i + paramStart, parameters[i].Name, false, true));
			}
		}

		private Module module;
		/// <summary>
		/// Gets the module that the method is being built for.
		/// </summary>
		public Module Module { get { return module; } }

		///// <summary>
		///// Gets the standard module for the current module pool. Convenience property.
		///// </summary>
		//public Module StandardModule { get { return module.Pool.StandardModule; } }

		private List<LocalVariable> parameters = new List<LocalVariable>();
		/// <summary>
		/// Gets the number of parameters that are in the method.
		/// </summary>
		public int ParamCount { get { return parameters.Count; } }

		private List<LocalVariable> locals = new List<LocalVariable>();
		/// <summary>
		/// Gets the number of local variables that the method contains.
		/// </summary>
		public int LocalCount { get { return locals.Count; } }

		private List<Instruction> instructions = new List<Instruction>();
		internal Instruction LastInstruction { get { return instructions.Count == 0 ? null : instructions[instructions.Count - 1]; } }

		/// <summary>
		/// A list of <see cref="Label"/> instances that have not been attached to any <see cref="Instruction"/>s.
		/// When an instruction is added to the <see cref="MethodBuilder"/>, these labels are all attached to that
		/// instruction, enabling the label to be used as a branch target.
		/// </summary>
		private List<Label> trailingLabels = new List<Label>();
		/// <summary>Mapping from label to instruction, for simplicity and speediness.</summary>
		private Dictionary<Label, Instruction> labelToInstr = new Dictionary<Label, Instruction>();
		/// <summary>
		/// A stack of compiler "states", such as loops, try blocks, whatever else the compiler
		/// may have pushed on here for its own sake.
		/// </summary>
		private Stack<object> states = new Stack<object>();

		private List<TryBlock> tryBlocks;

		// Try blocks that do not have their first instruction yet.
		private Stack<TryBlockMember> newTryMembers = new Stack<TryBlockMember>();

		private int? maxStack = null;

		/// <summary>
		/// Appends an instruction to the end of the method.
		/// </summary>
		/// <param name="instr">The instruction to append.</param>
		/// <exception cref="ArgumentNullException"><paramref name="instr"/> is null.</exception>
		/// <exception cref="ArgumentException"><paramref name="instr"/> is already in the method.
		/// -or-
		/// <paramref name="instr"/> is used in another method.</exception>
		public void Append(Instruction instr)
		{
			if (instr == null)
				throw new ArgumentNullException("instr");
			if (instr.Parent == this)
				throw new ArgumentException("The specified instruction instance is already in the method.");
			if (instr.Parent != null)
				throw new ArgumentException("The specified instruction instance already belongs to another method.");

			instr.Index = instructions.Count;
			instructions.Add(instr);
			instr.Parent = this;
			if (trailingLabels.Count > 0)
			{
				foreach (var label in trailingLabels)
				{
					instr.AddLabel(label);
					labelToInstr.Add(label, instr);
				}
				trailingLabels.Clear();
			}

			while (newTryMembers.Count > 0)
				newTryMembers.Pop().BeginBlock(instr);

			maxStack = null;
		}
		/// <summary>
		/// Appends a label to the end of the method.
		/// </summary>
		/// <param name="label">The label to append to the method.</param>
		/// <exception cref="ArgumentNullException"><paramref name="label"/> is null.</exception>
		public void Append(Label label)
		{
			if (label == null)
				throw new ArgumentNullException("label");
			trailingLabels.Add(label);
		}

		/// <summary>
		/// Begins a try block in the method. The next appended instruction will be the first instruction of the block.
		/// </summary>
		public TryBlock BeginTry()
		{
			TryBlock result;
			if (tryBlocks != null && !tryBlocks[tryBlocks.Count - 1].IsFinished)
			{
				var current = tryBlocks[tryBlocks.Count - 1];
				result = new TryBlock(this, current);

				switch (current.Kind)
				{
					case TryBlockKind.TryCatch:
						current.Catches[current.Catches.Count - 1].AddChildTryBlock(result);
						break;
					case TryBlockKind.TryFinally:
						current.Finally.AddChildTryBlock(result);
						break;
					default: // Invalid; try not ended yet
						current.AddChildTryBlock(result);
						break;
				}
			}
			else
			{
				if (tryBlocks == null)
					tryBlocks = new List<TryBlock>();

				result = new TryBlock(this, null);
				tryBlocks.Add(result);
			}

			result.BeginBlock();
			return result;
		}

		internal void AddNewTryMember(TryBlockMember member)
		{
			newTryMembers.Push(member);
		}

		/// <summary>
		/// Gets the parameter with the specified name.
		/// </summary>
		/// <param name="name">The name of the parameter to retrieve.</param>
		/// <returns>The parameter with the specified name.</returns>
		/// <exception cref="ArgumentError">A parameter with the name <paramref name="name"/> was not found.</exception>
		public LocalVariable GetParameter(string name)
		{
			if (name == null)
				throw new ArgumentNullException("name");

			for (var i = 0; i < parameters.Count; i++)
				if (parameters[i].Name == name)
					return parameters[i];

			throw new ArgumentException(string.Format("Could not find parameter: {0}", name));
		}
		/// <summary>
		/// Gets the parameter at the specified index.
		/// </summary>
		/// <param name="index">The index of the parameter to retrieve.</param>
		/// <returns>The parameter at the specified index.</returns>
		/// <exception cref="ArgumentOutOfRangeError"><paramref name="index"/> is less than zero.
		/// -or-
		/// <paramref name="index"/> is greater than or equal to <see cref="ParamCount"/>.</exception>
		public LocalVariable GetParameter(int index)
		{
			if (index < 0 || index >= parameters.Count)
				throw new ArgumentOutOfRangeException("index");

			return parameters[index];
		}

		/// <summary>
		/// Returns an anonymous local variable, which can be reused once it is no longer needed.
		/// </summary>
		/// <param name="name">(optional) The name of the local parameter. If null, then a default anonymous variable name will be generated.</param>
		/// <returns>A <see cref="LocalVariable"/> instance with <see cref="LocalVariable.IsAnonymous"/> set to true.</returns>
		public LocalVariable GetAnonymousLocal(string name = null)
		{
			LocalVariable local;
			// try first to find an anonymous local variable that is not in use
			for (var i = 0; i < locals.Count; i++)
			{
				local = locals[i];
				if (local.IsAnonymous && !local.InUse)
				{
					local.InUse = true; // and now we've started using it
					return local; // done!
				}
			}

			// at this point, we have not been able to find an unused anonymous local,
			// so we have to create one!
			local = new LocalVariable(locals.Count, GetTempLocalName(name, locals.Count), true, false);
			locals.Add(local);
			return local; // InUse starts out true
		}
		/// <summary>
		/// Gets the first non-anonymous local with the specified name, or adds one if it could not be found.
		/// </summary>
		/// <param name="name">The name of the local to look for.</param>
		/// <returns>The specified local, or a new instance if it could not be found.</returns>
		public LocalVariable GetLocal(string name)
		{
			if (name == null)
				throw new ArgumentNullException("name");

			LocalVariable local;
			for (var i = 0; i < locals.Count; i++)
			{
				local = locals[i];
				if (!local.IsAnonymous && local.Name == name)
					return local;
			}

			// At this point, the local could not be found. Let's add it!
			local = new LocalVariable(locals.Count, name, false, false);
			locals.Add(local);
			return local; // Note: InUse starts out true
		}

		/// <summary>
		/// Pushes a state onto the top of the state stack.
		/// </summary>
		/// <param name="state">The state to push.</param>
		/// <exception cref="ArgumentNullException"><paramref name="state"/> is null.</exception>
		public void PushState(object state)
		{
			if (state == null)
				throw new ArgumentNullException("state");
			states.Push(state);
		}
		/// <summary>
		/// Pops a state from the state stack, and returns it.
		/// </summary>
		/// <returns>The state that was popped from the state stack.</returns>
		public object PopState()
		{
			return states.Pop();
		}
		/// <summary>
		/// Pops a state from the state stack and checks it against an expected state.
		/// </summary>
		/// <param name="expected">The state to test against.</param>
		/// <returns>The state that was popped.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="expected"/> is null.</exception>
		/// <exception cref="CompileTimeException">The expectation was not met.</exception>
		public object PopState(object expected)
		{
			if (expected == null)
				throw new ArgumentNullException("expected");

			var state = states.Pop();
			if (!state.Equals(expected))
				throw new CompileTimeException(null, "The state stack contained an invalid state (expectation not met).");

			return state;
		}
		/// <summary>
		/// Returns thetstate at the top of the state stack without popping it.
		/// </summary>
		/// <returns>The state at the top of the state stack.</returns>
		public object PeekState()
		{
			return states.Peek();
		}
		/// <summary>
		/// Finds a state that matches the specified selector.
		/// </summary>
		/// <param name="selector">The selector to filter by. If it returns true, the state matches.</param>
		/// <returns>The state matching the selector, or null if none could be found.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="selector"/> is null.</exception>
		public object FindState(Func<object, bool> selector)
		{
			if (selector == null)
				throw new ArgumentNullException("selector");

			// Note: stacks are enumerated top to bottom, just like we want.
			foreach (var state in states)
				if (selector(state))
					return state;

			return null; // Not found
		}
		/// <summary>
		/// Finds a state that matches the specified selector.
		/// </summary>
		/// <typeparam name="T">The state type for which the selector is invoked. States of other types are ignored.</typeparam>
		/// <param name="selector">The selector to filter by. If it returns true, the state matches.</param>
		/// <returns>True if a matching state was found; otherwise, false.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="selector"/> is null.</exception>
		public bool FindState<T>(Func<T, bool> selector, out T result)
		{
			if (selector == null)
				throw new ArgumentNullException("selector");

			foreach (var state in states)
				if (state is T && selector((T)state))
				{
					result = (T)state;
					return true;
				}

			result = default(T);
			return false;
		}

		private Instruction FindInstruction(Instruction original, Label label)
		{
			var visited = new List<Instruction> { original };
			while (true)
			{
				Instruction instr;
				if (labelToInstr.TryGetValue(label, out instr))
				{
					// Branching to an unconditional branch makes the MethodBuilder forward directly
					// to the target of the unconditional branch.

					var branchInstr = instr as Branch;
					if (branchInstr != null && branchInstr.Condition == BranchCondition.Unconditional &&
						!visited.Contains(instr))
					{
						label = branchInstr.Target;
						visited.Add(branchInstr);
						continue; // next iteration, whoo!
					}
					return instr;
				}
				else
					break; // Nope
			}
			throw new UnresolvedLabelException(label);
		}

		/// <summary>
		/// Gets the bytes representing the current method in the method builder.
		/// </summary>
		/// <returns>An array of bytes.</returns>
		public byte[] GetBodyBytes()
		{
			if (trailingLabels.Count > 0)
				throw new InvalidOperationException("There are one or more trailing labels in the program. Every label must be followed by an instruction, otherwise there is nothing to branch to for instructions that reference the label.");

			// First pass: walk through all the instructions.
			// During this pass, initialize all the target instructions of
			// branches and switches, and assume they're all short jumps.
			// Also set the (initial) byteOffset of each instruction.
			var branches = new List<Instruction>();
			var offset = 0;
			foreach (var instr in instructions)
			{
				if (instr is Branch)
				{
					var br = instr as Branch;
					if (br.targetInstr == null)
						br.targetInstr = FindInstruction(br, br.Target);
					branches.Add(br);
				}
				else if (instr is Switch)
				{
					var sw = instr as Switch;
					var targets = sw.targets;
					for (var k = 0; k < targets.Count; k++)
					{
						var target = targets[k];
						if (target.Instr == null)
							target.Instr = FindInstruction(sw, target.Label);
						targets[k] = target;
					}
					branches.Add(sw);
				}

				instr.ByteOffset = offset;
				offset += instr.GetSize();
			}

			// If the method actually has any branching going on, then we need to do a few more things!
			if (branches.Count > 0)
			{
				// Second pass: calculate the offset of each target instruction
				// for each branch and switch instruction.
				// Remember instructions that definitely need to be long jumps.
				//
				// The logic is as follows:
				//   1. Initially, we assume that all jumps are short.
				//   2. After calculating the initial jump offsets, if we find that any jump instruction
				//      needs to be a long jump, that fact will not change.
				//   3. After turning any necessary short jumps into long jumps, the size of each such
				//      instruction will have changed (long jumps take an int argument, short jumps sbyte).
				//   4. Therefore, we need to recalculate the byte offset of every instruction.
				//   5. After recalculating said byte offsets, we recalculate the jump offsets.
				//   6. Because the jump offsets have now changed, we also need to re-test each jump
				//      instruction to make sure we don't have any new long jumps.
				//   7. If we do, repeat from step 4.
				// An effect of this is that it is NOT possible to go from long to short jump.
				// Note that we don't need to do any of this if there is no branching going on in the method.
				bool hasNewLongJumps = false;

				do
				{
					if (hasNewLongJumps)
					{
						// We need to recalculate the byte offset of every instruction.
						// Lame. :<
						// Let's hope it's not a really long method!
						offset = 0;
						foreach (var instr in instructions)
						{
							instr.ByteOffset = offset;
							offset += instr.GetSize();
						}
					}

					hasNewLongJumps = false;
					// And then we recalculate the offsets of every branch. If any branch goes from short to long,
					// we set hasNewLongJumps to true in the method, and recalculate everything again.
					foreach (var instr in branches)
						if (instr is Branch)
							ProcessBranchOffset((Branch)instr, ref hasNewLongJumps);
						else if (instr is Switch)
							ProcessSwitchOffsets((Switch)instr, ref hasNewLongJumps);

					// Note: at this stage, if hasNewLongJumps is false, then every branch instruction has an accurate
					// jump offset and every instruction has an accurate byte offset.
					// If it is true, however, we need to recalculate all the byte offsets, because one or more instructions
					// have become longer than before, and then we need to recalculate every single jump offset as well.
					// This is a naïve solution compared to building a complex branch tree and only updating what needs
					// to be updated, and may result in several loops being run, but in practice, I suspect most methods
					// are simple enough that it won't actually make any noticeable difference. Not to mention you only need
					// to run this once per method per compilation.
				} while (hasNewLongJumps);
			}

			// Now that we're done with everything above, we can actually output some bytes!
			// Note that we know the length of the output: it is the current value of 'offset'.
			var output = new byte[offset];
			foreach (var instr in instructions)
			{
				var bytes = instr.GetBytes();
#if DEBUG
				if (bytes.Length != instr.GetSize())
					throw new CompileTimeException(null,
						"The length of an instruction's byte array did not match the reported size of the instruction.");
#endif
				bytes.CopyTo(output, instr.ByteOffset);
			}

			return output;
		}

		public TryBlock[] GetTryBlocks()
		{
			if (tryBlocks == null)
				return null;

			foreach (var @try in tryBlocks)
				@try.Lock();

			return tryBlocks.ToArray();
		}

		public int GetMaxStack()
		{
			if (this.maxStack.HasValue)
				return this.maxStack.Value;

			// Index -> expected stack count of visited instructions;
			// -1 means the instruction hasn't been visited yet
			var knownCounts = new int[instructions.Count];
			for (var i = 0; i < instructions.Count; i++)
				knownCounts[i] = -1;

			var branches = new Queue<BranchDescription>();

			// Begin at the first instruction
			branches.Enqueue(new BranchDescription(target: 0, stackCount: 0));

			if (tryBlocks != null)
			{
				var tries = new Queue<TryBlock>(tryBlocks);

				while (tries.Count > 0)
				{
					var @try = tries.Dequeue();
					if (@try.HasChildTryBlocks)
						tries.EnqueueRange(@try.childTryBlocks);

					if (@try.Kind == TryBlockKind.TryFinally)
					{
						var @finally = @try.Finally;
						@finally.Start.AddRestriction(InstructionRestrictions.CannotFallThroughTo);
						branches.Enqueue(new BranchDescription(@finally.Start.Index, 0));
						if (@finally.HasChildTryBlocks)
							tries.EnqueueRange(@finally.childTryBlocks);
					}
					else // TryCatch
						foreach (var @catch in @try.Catches)
						{
							@catch.Start.AddRestriction(InstructionRestrictions.CannotFallThroughTo);
							branches.Enqueue(new BranchDescription(@catch.Start.Index, 1));
							if (@catch.HasChildTryBlocks)
								tries.EnqueueRange(@catch.childTryBlocks);
						}
				}
			}

			var maxStack = 0;

			while (branches.Count > 0)
			{
				var branch = branches.Dequeue();

				while (true) // Keep going until we hit an already visited instruction or ret/retnull
				{
					var instr = instructions[branch.Index];
					if (knownCounts[branch.Index] != -1)
					{
						if (knownCounts[branch.Index] != branch.StackCount)
							throw new InvalidOperationException(
								string.Format("Instruction #{0} ('{1}') reached with different stack sizes.",
									branch.Index, instr.ToString()));
						break; // Already visited this branch
					}

					// Store the index + the stack count upon reaching the instruction
					knownCounts[branch.Index] = branch.StackCount;

					branch.StackCount = GetStackAfter(branch.StackCount, instr);
					if (branch.StackCount > maxStack)
						maxStack = branch.StackCount;

					if (instr is Branch)
					{
						var branchInstr = (Branch)instr;
						var targetIndex = instructions.IndexOf(branchInstr.targetInstr);
						if (branchInstr.Condition == BranchCondition.Unconditional ||
							branchInstr.Condition == BranchCondition.Leave)
						{
							branch.Index = branchInstr.targetInstr.Index;
							continue;
						}
						branches.Enqueue(new BranchDescription(targetIndex, branch.StackCount));
					}
					else if (instr is Switch)
					{
						foreach (var target in ((Switch)instr).targets)
						{
							var targetIndex = instructions.IndexOf(target.Instr);
							branches.Enqueue(new BranchDescription(targetIndex, branch.StackCount));
						}
					}
					else if (instr is SimpleInstruction)
					{
						var simple = (SimpleInstruction)instr;
						if (simple.Opcode == Opcode.Ret || simple.Opcode == Opcode.Retnull ||
							simple.Opcode == Opcode.Throw || simple.Opcode == Opcode.Rethrow ||
							simple.Opcode == Opcode.Endfinally)
							break; // End of the line!
					}

					branch.Index++;
					if (branch.Index >= instructions.Count)
						throw new InvalidOperationException("Fell through the end of the method.");
					if (instructions[branch.Index].HasRestriction(InstructionRestrictions.CannotFallThroughTo))
						throw new InvalidOperationException(
							string.Format("Instruction #{0} ('{1}') should not be reached by fallthrough.",
								branch.Index, instructions[branch.Index]));
				}
			}

			this.maxStack = maxStack;
			return maxStack;
		}

		public string ToStringFull()
		{
			return ToStringFull(null);
		}

		private string ToStringFull(int[] knownStackCounts)
		{
			var sb = new StringBuilder(instructions.Count * 8 + 32);

			if (maxStack.HasValue)
			{
				sb.Append(".maxstack ");
				sb.AppendLine(maxStack.Value.ToString("D"));
			}

			if (locals.Count > 0)
			{
				sb.Append(".locals ( ");
				for (var i = 0; i < locals.Count; i++)
				{
					if (i > 0)
						sb.Append(", ");
					sb.Append(locals[i].Name ?? "(unnamed)");
				}
				sb.AppendLine(" )");
			}

			if (tryBlocks != null)
			{
				Action<StringBuilder, List<TryBlock>> printTries = null;
				printTries = (sbInner, tries) =>
				{
					foreach (var @try in tries)
					{
						// Print inner to outer
						if (@try.HasChildTryBlocks)
							printTries(sbInner, @try.childTryBlocks);

						sbInner.AppendFormat(".try (OV_{0:X4}, OV_{1:X4})",
							@try.StartOffset, @try.EndOffset);
						if (@try.Kind == TryBlockKind.TryFinally)
						{
							sbInner.AppendFormat(" finally (OV_{0:X4}, OV_{1:X4})",
								@try.Finally.StartOffset, @try.Finally.EndOffset);

							if (@try.Finally.HasChildTryBlocks)
								printTries(sbInner, @try.Finally.childTryBlocks);
						}
						else
						{
							var childTries = new Queue<List<TryBlock>>();
							foreach (var @catch in @try.Catches)
							{
								sbInner.AppendFormat(" catch {0:X8} [{1}] (OV_{2:X4}, OV_{3:X4})",
									@catch.TypeId, module.GetType(@catch.TypeId).FullName,
									@catch.StartOffset, @catch.EndOffset);
								if (@catch.HasChildTryBlocks)
									childTries.Enqueue(@catch.childTryBlocks);
							}
							while (childTries.Count > 0)
								printTries(sbInner, childTries.Dequeue());
						}
						sbInner.AppendLine();
					}
				};

				printTries(sb, tryBlocks);
			}

			for (var i = 0; i < instructions.Count; i++)
			{
				var instr = instructions[i];
				if (knownStackCounts != null)
					sb.AppendFormat("[{2} -{0}+{1}] ", instr.StackChange.Removed, instr.StackChange.Added, knownStackCounts[i]);
				sb.AppendFormat("OV_{0:X4}: ", instr.ByteOffset);
				sb.AppendLine(instr.ToString());
			}

			return sb.ToString();
		}

		private static void ProcessBranchOffset(Branch br, ref bool hasNewLongJumps)
		{
			br.targetOffset = br.targetInstr.ByteOffset - br.ByteOffset - br.GetSize();
			if (br.isShortJump && (br.targetOffset < sbyte.MinValue || br.targetOffset > sbyte.MaxValue))
			{
				hasNewLongJumps = true;
				br.isShortJump = false;
			}
		}

		private static void ProcessSwitchOffsets(Switch sw, ref bool hasNewLongJumps)
		{
			var targets = sw.targets;
			var offsetAfter = sw.ByteOffset + sw.GetSize();
			for (var k = 0; k < targets.Count; k++)
			{
				var target = targets[k];
				target.Offset = target.Instr.ByteOffset - offsetAfter;

				if (sw.isShortJump && (target.Offset < sbyte.MinValue || target.Offset > sbyte.MaxValue))
				{
					hasNewLongJumps = true;
					sw.isShortJump = false;
				}

				targets[k] = target;
			}
		}

		private static string GetTempLocalName(string nameHint, int index)
		{
			return string.Format("<{0}>${1:D4}", nameHint ?? "Osp", index);
		}

		private static int GetStackAfter(int stackBefore, Instruction instr)
		{
			if (instr is Branch && ((Branch)instr).Condition == BranchCondition.Leave)
				return 0; // leave/leave.s always empties the stack, regardless of what's on it
			if (instr is SimpleInstruction)
			{
				var opcode = ((SimpleInstruction)instr).Opcode;
				if (opcode == Opcode.Ret && stackBefore != 1)
					throw new InvalidOperationException("ret requires exactly one value on the stack.");
				if (opcode == Opcode.Retnull && stackBefore != 0)
					throw new InvalidOperationException("retnull requires exactly zero values on the stack.");
			}
			var result = stackBefore + instr.StackChange;
			if (result < 0)
				throw new InvalidOperationException(string.Format("Not enough values on stack for instruction '{0}'.",
					instr.ToString()));

			return result;
		}

		private struct BranchDescription
		{
			public BranchDescription(int target, int stackCount)
			{
				Index = target;
				StackCount = stackCount;
			}

			public int Index;
			public int StackCount;
		}
	}

	/// <summary>
	/// Represents an error condition where a label could not be matched to an instruction within a method.
	/// </summary>
	public class UnresolvedLabelException : CompileTimeException
	{
		public UnresolvedLabelException() : this(null)
		{ }
		public UnresolvedLabelException(Label label)
			: this(label, "A label could not be matched to an instruction.", null)
		{ }
		public UnresolvedLabelException(Label label, string message)
			: this(label, message, null)
		{ }
		public UnresolvedLabelException(Label label, string message, Exception innerException)
			: base(null, message, innerException)
		{
			this.label = label;
		}

		private Label label;
		/// <summary>
		/// Gets the label that could not be resolved.
		/// </summary>
		public Label Label { get { return label; } }
	}

	/// <summary>
	/// Represents a local variable.
	/// </summary>
	public class LocalVariable
	{
		public LocalVariable(int index)
			: this(index, null, true, false)
		{ }
		public LocalVariable(int index, string name)
			: this(index, name, false, false)
		{ }
		internal LocalVariable(int index, string name, bool isAnonymous, bool isParam)
		{
			if (isParam && isAnonymous)
				throw new InvalidOperationException("An argument cannot be marked anonymous.");
			//if ((isParam || !isAnonymous) && (name == null || name.Length == 0))
			//	throw new ArgumentException("Arguments and non-anonymous locals cannot have an empty name");
			this.index = index;
			this.name = name;
			this.flags = (isParam ? LocalFlags.IsParam : LocalFlags.InUse) | (isAnonymous ? LocalFlags.IsAnonymous : LocalFlags.None);
		}

		int index;
		/// <summary>
		/// Gets the index of the local variable within the method.
		/// </summary>
		public int Index { get { return index; } }

		string name;
		/// <summary>
		/// Gets the name of the local variable, or null if it has none.
		/// </summary>
		public string Name { get { return name; } }

		private LocalFlags flags;

		/// <summary>
		/// Gets a value indicating whether the local is currently in use.
		/// </summary>
		public bool InUse
		{
			get { return (flags & LocalFlags.InUse) == LocalFlags.InUse; }
			internal set
			{
				if (IsParam)
					throw new InvalidOperationException("Arguments cannot be marked as being in use.");
				if (!IsAnonymous)
					throw new InvalidOperationException("Non-anonymous variables cannot be marked as being in use.");

				if (value)
					flags |= LocalFlags.InUse;
				else
					flags &= ~LocalFlags.InUse;
			}
		}

		/// <summary>
		/// Gets a value indicating whether the local variable is anonymous.
		/// </summary>
		public bool IsAnonymous { get { return (flags & LocalFlags.IsAnonymous) == LocalFlags.IsAnonymous; } }

		/// <summary>
		/// Gets a value indicating whether the local variable refers to an argument.
		/// </summary>
		public bool IsParam { get { return (flags & LocalFlags.IsParam) == LocalFlags.IsParam; } }

		/// <summary>
		/// Notifies the local variable that it is no longer needed.
		/// This sets the <see cref="InUse"/> property to false, allowing it
		/// to be reused if it is an anonymous local variable.
		/// </summary>
		public void Done()
		{
			InUse = false;
		}

		/// <summary>
		/// Makes a <see cref="LocalVariable"/> that represents a parameter/argument.
		/// </summary>
		/// <param name="index">The index of the parameter.</param>
		/// <param name="name">The name of the parameter.</param>
		/// <returns>A new <see cref="LocalVariable"/> instance representing the specified argument/parameter.</returns>
		internal static LocalVariable MakeParam(int index, string name)
		{
			return new LocalVariable(index, name, false, true);
		}

		[Flags]
		public enum LocalFlags
		{
			/// <summary>Specifies no flags.</summary>
			None = 0,
			/// <summary>The local variable is in use (and cannot be reused).</summary>
			InUse = 1,
			/// <summary>The local variable is anonymous (and can be reused when no longer needed).</summary>
			IsAnonymous = 2,
			/// <summary>The local variable refers to a parameter.</summary>
			IsParam = 4,
		}
	}

	/// <summary>
	/// Represents a jump label in a method.
	/// </summary>
	/// <remarks>
	/// Labels are compared for equality by reference, and are therefore hashed as such as well.
	/// The original object.GetHashCode and object.Equals implementations do this, so we don't override them.
	/// </remarks>
	public class Label
	{
		public Label() : this(null)
		{ }
		/// <summary>
		/// Initializes a new <see cref="Label"/> with the specified name.
		/// </summary>
		/// <param name="name">The name of the label.</param>
		public Label(string name)
		{
			this.name = name;
		}

		private string name;
		/// <summary>
		/// Gets the name of the label.
		/// </summary>
		/// <remarks>
		/// This name exists purely for readability and debuggability purposes.
		/// Labels are compared for equality by reference, not by name.
		/// </remarks>
		public string Name { get { return name; } }

		public override string ToString()
		{
			return name != null ? string.Format("Label: {0}", name) : "Label (unnamed)";
		}
	}
}