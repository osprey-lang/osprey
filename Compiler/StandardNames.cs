using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	internal static class StandardNames
	{
		public const string StandardModuleName = "aves";

		public const string StandardNamespace = "aves";
		public const string TypeRootName = StandardNamespace + ".Object";

		public const string EnumName = StandardNamespace + ".Enum";
		public const string EnumSetName = StandardNamespace + ".EnumSet";

		public const string BooleanName = StandardNamespace + ".Boolean";
		public const string IntName = StandardNamespace + ".Int";
		public const string UIntName = StandardNamespace + ".UInt";
		public const string RealName = StandardNamespace + ".Real";
		public const string StringName = StandardNamespace + ".String";

		// And these are some other standard types
		public const string ListName = StandardNamespace + ".List";
		public const string HashName = StandardNamespace + ".Hash";
		public const string IteratorName = StandardNamespace + ".Iterator";
		public const string MethodName = StandardNamespace + ".Method";
		public const string ErrorName = StandardNamespace + ".Error";
		public const string TypeName = StandardNamespace + ".Type";

		public static string GetStandardName(string name)
		{
			return StandardNamespace + "." + name;
		}
	}
}