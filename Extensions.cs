using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;
using CI = System.Globalization.CultureInfo;

namespace Osprey
{
	internal static class Extensions
	{
		public static string JoinString<T>(this IEnumerable<T> collection)
		{
			if (collection == null)
				return null;

			var sb = new StringBuilder();
			foreach (var value in collection)
			{
				sb.Append(value.ToString());
			}
			return sb.ToString();
		}

		public static string JoinString<T>(this IEnumerable<T> collection, string separator)
		{
			if (collection == null)
				return null;

			var sb = new StringBuilder();
			var needsSep = false;
			foreach (var value in collection)
			{
				if (needsSep)
					sb.Append(separator);
				else
					needsSep = true;
				sb.Append(value.ToString());
			}
			return sb.ToString();
		}

		public static string JoinString(this IEnumerable<ParseNode> collection, string separator, int indent)
		{
			if (collection == null)
				return null;

			var sb = new StringBuilder();
			var needsSep = false;
			foreach (var value in collection)
			{
				if (needsSep)
					sb.Append(separator);
				else
					needsSep = true;
				sb.Append(value.ToString(indent));
			}
			return sb.ToString();
		}

		public static void Reverse<T>(this T[] array)
		{
			var iMax = array.Length >> 1;
			for (var i = 0; i < iMax; i++)
			{
				var iOpposite = array.Length - i;
				var temp = array[i];
				array[i] = array[iOpposite];
				array[iOpposite] = temp;
			}
		}

		public static bool IsInFieldInitializer(this IDeclarationSpace context)
		{
			if (context == null)
				throw new ArgumentNullException("context");

			// If we encounter a Class before a Method (unless it's a LocalMethod),
			// then it must be a field initializer.
			// Examples:
			//   class C {
			//      public x = "chirp";          context: Class
			//      public y = @= "birds";       context: LocalMethod <- Class
			//      public z() {                 context: Method <- Class
			//         function i() {            context: LocalMethod <- Method <- Class
			//            return "feathers";
			//         }
			//         return i();
			//      }
			//   }
			do
			{
				if (context is Method && !(context is LocalMethod))
					return false;
				if (context is Class)
					return true;
				context = context.Parent;
			} while (context !=	null);

			return false;
		}

		public static bool HasRefArguments(this IEnumerable<Expression> collection)
		{
			if (collection == null)
				throw new ArgumentNullException("collection");

			return collection.Any(expr => expr is RefExpression);
		}

		private static void CopyBytesInternal<T>(T[] array, byte[] bytes, int offset)
		{
			if (!BitConverter.IsLittleEndian)
				bytes.Reverse();
			bytes.CopyTo(array, offset);
		}
		public static void CopyBytes<T>(this T[] array, char value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, short value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, int value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, long value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, ushort value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, uint value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, ulong value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}
		public static void CopyBytes<T>(this T[] array, double value, int offset)
		{
			CopyBytesInternal(array, BitConverter.GetBytes(value), offset);
		}

		public static void EnqueueRange<T>(this Queue<T> queue, IEnumerable<T> values)
		{
			foreach (var value in values)
				queue.Enqueue(value);
		}

		public static string ToJsonString(this string value)
		{
			if (value == null)
				throw new ArgumentNullException("value");

			var sb = new StringBuilder("\"");

			for (var i = 0; i < value.Length; i++)
			{
				var ch = value[i];
				var isSurrogate = char.IsSurrogatePair(value, i);
				var needToEscape = isSurrogate ||
					char.IsControl(value, i) ||
					jsonEscapes.ContainsKey(ch);

				if (needToEscape)
				{
					if (jsonEscapes.ContainsKey(ch))
						sb.Append(jsonEscapes[ch]);
					else
					{
						sb.AppendFormat("\\u{0:x4}", ch);
						if (isSurrogate)
							sb.AppendFormat("\\u{0:x4}", value[i + 1]);
					}
				}
				else
				{
					sb.Append(ch);
					if (isSurrogate)
						sb.Append(value[i + 1]);
				}

				if (isSurrogate)
					i++;
			}

			sb.Append("\"");

			return sb.ToString();
		}

		public static string ToJsonString(this int value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToJsonString(this uint value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToJsonString(this long value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToJsonString(this ulong value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToJsonString(this double value)
		{
			if (double.IsNaN(value) || double.IsInfinity(value))
				throw new ArgumentException("JSON cannot represent NaN or infinity.", "value");
			return value.ToString(CI.InvariantCulture);
		}

		private static Dictionary<char, string> jsonEscapes = new Dictionary<char, string>
		{
			{'"', "\\\""}, {'\\', "\\\\"},
			{'/', "\\/"}, {'\b', "\\b"},
			{'\f', "\\f"}, {'\n', "\\n"},
			{'\r', "\\r"}, {'\t', "\\t"},
			{'\u2028', "\\u2028"}, // Line Separator
			{'\u2029', "\\u2029"}, // Paragraph separator
		};

		public static string ToStringInvariant(this int value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToStringInvariant(this long value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToStringInvariant(this ulong value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToStringInvariant(this double value)
		{
			return value.ToString(CI.InvariantCulture);
		}

		public static string ToStringInvariant(this Version value, int fieldCount)
		{
			switch (fieldCount)
			{
				case 0:
					return "";
				case 1:
					return value.Major.ToStringInvariant();
				case 2:
					return value.Major.ToStringInvariant() + "." + value.Minor.ToStringInvariant();
				case 3:
					return string.Concat(
						value.Major.ToStringInvariant(), ".",
						value.Minor.ToStringInvariant(), ".",
						value.Build.ToStringInvariant()
					);
				case 4:
					return string.Concat(
						value.Major.ToStringInvariant(), ".",
						value.Minor.ToStringInvariant(), ".",
						value.Build.ToStringInvariant(), ".",
						value.Revision.ToStringInvariant()
					);
				default:
					throw new ArgumentOutOfRangeException("fieldCount");
			}
		}

		public static bool IsRead(this ExpressionAccessKind access)
		{
			return (access & ExpressionAccessKind.Read) == ExpressionAccessKind.Read;
		}

		public static bool IsWrite(this ExpressionAccessKind access)
		{
			return (access & ExpressionAccessKind.Write) == ExpressionAccessKind.Write;
		}
	}
}