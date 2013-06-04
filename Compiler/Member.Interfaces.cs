using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Nodes;

namespace Osprey.Members
{
	/// <summary>
	/// Represents a declaration space, which contains named members.
	/// </summary>
	public interface IDeclarationSpace
	{
		/// <summary>
		/// Gets the parent of the declaration space, or null if there is none.
		/// </summary>
		IDeclarationSpace Parent { get; }

		/// <summary>
		/// Determines whether the declaration space contains the specified member.
		/// </summary>
		/// <param name="name">The name of the member to look up.</param>
		/// <returns>True if the declaration space contains the member; otherwise, false.</returns>
		bool ContainsMember(string name);

		/// <summary>
		/// Attempts to resolve a name in a given declaration space.
		/// </summary>
		/// <param name="name">The name to resolve.</param>
		/// <param name="fromClass">The class containing the node with the name reference, or null if the node does not occur inside a class.</param>
		/// <returns>The member that the name refers to, or null if it could not be found.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="name"/> is null.</exception>
		/// <remarks>Note that this uses <see cref="Class"/> instead of <see cref="Osprey.Members.Type"/>,
		/// because enums only have public members.</remarks>
		NamedMember ResolveName(string name, Class fromClass);

		/// <summary>
		/// Gets the namespace the declaration space is ultimately contained within.
		/// If the declaration space is already a namespace, then this method must return
		/// the parent of that namespace.
		/// </summary>
		/// <returns>The namespace that contains the declaration space, or null if the declaration space is the global declaration space.</returns>
		Namespace GetContainingNamespace();

		/// <summary>
		/// Gets the class the declaration space is ultimately contained within.
		/// If the declaration space is a class, then this method must return null.
		/// If the declaration space is not contained within a class, null is returned as well.
		/// </summary>
		/// <param name="hasInstance">(Out) If the context has access to instance members of the class
		/// (i.e. the context is inside an instance member of the class), this is set to true; otherwise, false.</param>
		/// <returns>The class containing the declaration space, or null if the declaration space is not contained within a class.</returns>
		Class GetContainingClass(out bool hasInstance);
	}

	/// <summary>
	/// Represents a member with a constant value.
	/// </summary>
	public interface IConstantMember
	{
		/// <summary>
		/// Gets the state of the constant, i.e. whether it has been reduced or not.
		/// </summary>
		ConstantState State { get; }
		/// <summary>
		/// Gets the value associated with the constant.
		/// </summary>
		ConstantValue Value { get; }
	}
}