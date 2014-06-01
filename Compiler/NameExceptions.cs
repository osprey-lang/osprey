using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;

namespace Osprey
{
	/// <summary>
	/// Thrown when a member declaration is invalid because there is already a member with the specified name.
	/// </summary>
	public class DuplicateNameException : CompileTimeException
	{
		public DuplicateNameException(ParseNode node, string name)
			: this(node, name, "There is already a member with the name '" + name + "'.", null)
		{ }
		public DuplicateNameException(ParseNode node, string name, string message)
			: this(node, name, message, null)
		{ }
		public DuplicateNameException(ParseNode node, string name, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.name = name;
		}

		private string name;
		/// <summary>
		/// Gets the name of the member that was declared multiple times.
		/// </summary>
		public string Name { get { return name; } }
	}

	/// <summary>
	/// Thrown when a name has not been defined.
	/// </summary>
	public class UndefinedNameException : CompileTimeException
	{
		public UndefinedNameException(ParseNode node, string name)
			: this(node, name, "The name '" + name + "' is not defined in this context.", null)
		{ }
		public UndefinedNameException(ParseNode node, string name, string message)
			: this(node, name, message, null)
		{ }
		public UndefinedNameException(ParseNode node, string name, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.name = name;
		}

		private string name;
		/// <summary>
		/// Gets the name that could not be resolved.
		/// </summary>
		public string Name { get { return name; } }
	}

	public class AmbiguousNameException : CompileTimeException
	{
		public AmbiguousNameException(ParseNode node, AmbiguousMember member)
			: this(node, member, "A name could not be resolved because it is ambiguous.", null)
		{ }
		public AmbiguousNameException(ParseNode node, AmbiguousMember member, string message)
			: this(node, member, message, null)
		{ }
		public AmbiguousNameException(ParseNode node, AmbiguousMember member, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.member = member;
		}

		private AmbiguousMember member;
		/// <summary>
		/// Gets an <see cref="AmbiguousMember"/> containing the members that the name could resolve to.
		/// </summary>
		public AmbiguousMember Member { get { return member; } }
	}

	public class AmbiguousTypeNameException : CompileTimeException
	{
		public AmbiguousTypeNameException(ParseNode node, AmbiguousTypeName name)
			: this(node, name, GetDefaultErrorMessage(name), null)
		{ }
		public AmbiguousTypeNameException(ParseNode node, AmbiguousTypeName name, string message)
			: this(node, name, message, null)
		{ }
		public AmbiguousTypeNameException(ParseNode node, AmbiguousTypeName name, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.name = name;
		}

		private AmbiguousTypeName name;
		/// <summary>
		/// Gets an <see cref="AmbiguousTypeName"/> containing the types that the name resolved to.
		/// </summary>
		public AmbiguousTypeName Name { get { return name; } }

		private static string GetDefaultErrorMessage(AmbiguousTypeName name)
		{
			var typeName = name.TypeName != null ?
				name.TypeName.Parts.JoinString(".") :
				null;
			return string.Format("The type name '{0}' is ambiguous between the following types: {1}",
				typeName ?? "(null)",
				AmbiguousMember.GetMemberNamesJoined(name.Types));
		}
	}

	/// <summary>
	/// Indicates that an invalid declaration occurred.
	/// </summary>
	public class DeclarationException : CompileTimeException
	{
		public DeclarationException(ParseNode node)
			: this(node, "An invalid member declaration was encountered.", null)
		{ }
		public DeclarationException(ParseNode node, string message)
			: this(node, message, null)
		{ }
		public DeclarationException(ParseNode node, string message, Exception innerException)
			: base(node, message, null)
		{ }
	}

    /// <summary>
    /// Indicates that a method group contains overloads with different declared accessibility,
	/// or that an overridden member has the wrong accessibility compared to its base member.
    /// </summary>
    public class InconsistentAccessibilityException : DeclarationException
    {
        public InconsistentAccessibilityException(ParseNode node)
            : this(node, "The overloads in the method group have different declared accessibility. All overloads must have the same declared accessibility.", null)
        { }
		public InconsistentAccessibilityException(ParseNode node, string message)
            : this(node, message, null)
        { }
		public InconsistentAccessibilityException(ParseNode node, string message, Exception innerException)
            : base(node, message, null)
        { }
    }

	/// <summary>
	/// Indicates that a group of overloads has ambguities.
	/// </summary>
	public class OverloadException : DeclarationException
	{
		/// <summary>
		/// Initializes a new <see cref="OverloadException"/> with a set of conflicting overloads.
		/// </summary>
		/// <param name="conflictingOverloads">An array of conflicting overloads.</param>
		public OverloadException(ParseNode node, params Member[] conflictingOverloads)
			: this(node, conflictingOverloads, "Two or more conflicting overloads were found.", null)
		{ }
		/// <summary>
		/// Initializes a new <see cref="OverloadException"/> with a set of conflicting overloads.
		/// </summary>
		/// <param name="conflictingOverloads">An array of conflicting overloads.</param>
		/// <param name="message">A message associated with the exception.</param>
		public OverloadException(ParseNode node, Member[] conflictingOverloads, string message)
			: this(node, conflictingOverloads, message, null)
		{ }
		/// <summary>
		/// Initializes a new <see cref="OverloadException"/> with a set of conflicting overloads.
		/// </summary>
		/// <param name="conflictingOverloads">An array of conflicting overloads.</param>
		/// <param name="message">A message associated with the exception.</param>
		/// <param name="innerException">The exception that caused this exception.</param>
		public OverloadException(ParseNode node, Member[] conflictingOverloads, string message, Exception innerException)
			: base(node, message, innerException)
		{
			if (conflictingOverloads == null)
				throw new ArgumentNullException("conflictingOverloads");
			this.conflictingOverloads = (Member[])conflictingOverloads.Clone();
		}

		private Member[] conflictingOverloads;
		/// <summary>
		/// Gets the overloads that are in conflict. A call to the method group would be ambiguous between these overloads.
		/// </summary>
		public Member[] ConflictingOverloads { get { return (Member[])conflictingOverloads.Clone(); } }
	}

	/// <summary>
	/// Thrown when an instance member is accessed statically.
	/// </summary>
	public class InstanceMemberAccessException : CompileTimeException
	{
		public InstanceMemberAccessException(ParseNode node, NamedMember member)
			: this(node, member, GetDefaultMessage(member), null)
		{ }
		public InstanceMemberAccessException(ParseNode node, NamedMember member, string message)
			: this(node, member, message, null)
		{ }
		public InstanceMemberAccessException(ParseNode node, NamedMember member, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.member = member;
		}

		private NamedMember member;
		/// <summary>
		/// Gets the member that was accessed incorrectly.
		/// </summary>
		public NamedMember Member { get { return member; } }

		private static string GetDefaultMessage(NamedMember member)
		{
			if (member != null)
				return string.Format("The member '{0}' must be accessed through an instance",
					member.FullName);
			return "Instance members must be accessed through an instance.";
		}
	}

	public class StaticMemberAccessException : CompileTimeException
	{
		public StaticMemberAccessException(ParseNode node, NamedMember member)
			: this(node, member, GetDefaultMessage(member), null)
		{ }
		public StaticMemberAccessException(ParseNode node, NamedMember member, string message)
			: this(node, member, message, null)
		{ }
		public StaticMemberAccessException(ParseNode node, NamedMember member, string message, Exception innerException)
			: base(node, message, innerException)
		{
			this.member = member;
		}

		private NamedMember member;
		/// <summary>
		/// Gets the member that was accessed incorrectly.
		/// </summary>
		public NamedMember Member { get { return member; } }

		private static string GetDefaultMessage(NamedMember member)
		{
			if (member != null)
				return string.Format("The member '{0}' is static and cannot be accessed through an instance.",
					member.FullName);
			return "Static members cannot be accessed through an instance.";
		}
	}
}