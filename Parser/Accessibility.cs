using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Represents the declared accessibility of a member.
	/// </summary>
	public enum Accessibility
	{
		/// <summary>
		/// The member has no explicitly declared accessibility.
		/// The default meaning depends on the kind of member; class members, for instance, are private by default.
		/// </summary>
		None = 0,
		/// <summary>
		/// The member is declared public.
		/// </summary>
		Public,
		/// <summary>
		/// The member is declared internal.
		/// </summary>
		Internal,
		/// <summary>
		/// The member is declared protected.
		/// </summary>
		Protected,
		/// <summary>
		/// The member is declared private.
		/// </summary>
		Private,
	}
}