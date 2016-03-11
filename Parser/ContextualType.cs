using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Represents the contextual type of an identifier token. In addition to being a plain
	/// identifier, some sequences of characters are used contextually as keywords. The standard
	/// language includes contextual keywords "to", "where" and "version". Extension keywords
	/// are also implemented as contextual keywords.
	/// </summary>
	public enum ContextualType
	{
		/// <summary>The identifier is not a contextual keyword.</summary>
		None = 0,
		/// <summary>to</summary>
		To = 1,
		/// <summary>where</summary>
		Where = 2,
		/// <summary>version</summary>
		Version = 3,
		/// <summary>__primitive</summary>
		Primitive = 4,
		/// <summary>__init_type</summary>
		InitType = 5,
		/// <summary>__extern</summary>
		Extern = 6,
		/// <summary>__named_const</summary>
		NamedConst = 7,
		/// <summary>__get_argc</summary>
		GetArgc = 8,
	}
}