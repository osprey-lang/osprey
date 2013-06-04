using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Instructions;
using Osprey.Members;
using Osprey.Nodes;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

namespace Osprey
{
	public partial class Compiler
	{
		/// <summary>
		/// Emits instructions to unpack a List value from the top of the stack into several local variables.
		/// </summary>
		/// <param name="method">The <see cref="MethodBuilder"/> to append the instructions to.</param>
		/// <param name="varNames">The names of the local variables to unpack into, in order from left to right.</param>
		internal void Unpack(MethodBuilder method, IEnumerable<string> varNames)
		{
			Unpack(method, varNames.Select(v => method.GetLocal(v)));
		}

		/// <summary>
		/// Emits instructions to unpack a List value from the top of the stack into several local variables.
		/// </summary>
		/// <param name="method">The <see cref="MethodBuilder"/> to append the instructions to.</param>
		/// <param name="varNames">The local variables to unpack into, in order from left to right.</param>
		/// <remarks>
		/// If any of the local variables in <paramref name="vars"/> are anonymous,
		/// the caller is responsible for marking them as unused.
		/// </remarks>
		internal void Unpack(MethodBuilder method, IEnumerable<LocalVariable> vars)
		{
			// The list value is on the top of the stack
			// Step 1: verify that it's actually a List value
			var listType = method.Module.GetTypeId(ListType);

			var unpackBody = new Label("unpack-main");

			method.Append(new SimpleInstruction(Opcode.Dup)); // Dup the list value
			method.Append(Branch.TypeEquals(unpackBody, listType)); // If value is aves.List, branch to main unpack body
			// Otherwise, throw
			{
				method.Append(new NewObject(method.Module.GetTypeId(TypeErrorType), 0)); // Create new TypeError
				method.Append(new SimpleInstruction(Opcode.Throw)); // Throw it
			}
			method.Append(unpackBody);
			// Remember: the list value is still on the top of the stack!

			// Step 2: walk through the targets and assign to them.
			var varsArr = vars.ToArray();
			for (var i = 0; i < varsArr.Length; i++)
			{
				if (varsArr[i] == null)
				{
					if (i == varsArr.Length - 1)
						method.Append(new SimpleInstruction(Opcode.Pop)); // Remove the value if the last thing here is null
					continue; // skip null entries
				}
				if (i < varsArr.Length - 1) // Only duplicate list value if this is not the last variable to be assigned to
					method.Append(new SimpleInstruction(Opcode.Dup));
				// List is on the top of the stack
				method.Append(new LoadConstantInt(i)); // Load the index
				method.Append(new LoadIndexer(1)); // Load list[i]
				method.Append(new StoreLocal(varsArr[i])); // variable = list[i]
			}
		}

		internal delegate void UnpackAssigner(MethodBuilder method, bool isAssignment);

		internal void Unpack(MethodBuilder method, IEnumerable<UnpackAssigner> assigners)
		{
			// (This is basically a copy of Unpack(MethodBuilder, IEnumerable<LocalVariable>),
			// with some modifications)
			// The list value is on the top of the stack
			// Step 1: verify that it's actually a List value
			var listType = method.Module.GetTypeId(ListType);

			var unpackBody = new Label("unpack-main");
			method.Append(new SimpleInstruction(Opcode.Dup)); // Dup the list value
			method.Append(Branch.TypeEquals(unpackBody, listType)); // If value is aves.List, branch to main unpack body
			// Otherwise, throw
			{
				method.Append(new NewObject(method.Module.GetTypeId(TypeErrorType), 0)); // Create new TypeError
				method.Append(new SimpleInstruction(Opcode.Throw)); // Throw it
			}
			method.Append(unpackBody);
			// Remember: the list value is still on the top of the stack!

			// Step 2: walk through the assigners, and assign each value to each of them.
			var assignersArr = assigners.ToArray();
			for (var i = 0; i < assignersArr.Length; i++)
			{
				if (i < assignersArr.Length - 1) // Only duplicate list value if this is not the last thing to be assigned to
					method.Append(new SimpleInstruction(Opcode.Dup));
				// List is on the top of the stack
				assignersArr[i](method, isAssignment: false); // Load the instance, if necessary
				method.Append(new LoadConstantInt(i)); // Load the index
				method.Append(new LoadIndexer(1)); // Load list[i]
				assignersArr[i](method, isAssignment: true); // Perform the assignment
			}
		}
	}
}