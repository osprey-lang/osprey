using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using CI = System.Globalization.CultureInfo;

namespace Osprey
{
	public class ModuleVersion : IComparable<ModuleVersion>, IEquatable<ModuleVersion>
	{
		public ModuleVersion(int major, int minor, int patch)
		{
			if (major < 0)
				throw new ArgumentOutOfRangeException("major");
			if (minor < 0)
				throw new ArgumentOutOfRangeException("minor");
			if (patch < 0)
				throw new ArgumentOutOfRangeException("patch");

			this.major = major;
			this.minor = minor;
			this.patch = patch;
		}

		private int major;
		public int Major { get { return major; } }

		private int minor;
		public int Minor { get { return minor; } }

		private int patch;
		public int Patch { get { return patch; } }

		public int CompareTo(ModuleVersion other)
		{
			if (other == null)
				return 1;

			if (this.major != other.major)
				return this.major < other.major ? -1 : 1;
			if (this.minor != other.minor)
				return this.minor < other.minor ? -1 : 1;
			if (this.patch != other.patch)
				return this.patch < other.patch ? -1 : 1;

			return 0;
		}

		public bool Equals(ModuleVersion other)
		{
			if (other == null)
				return false;
			return
				this.major == other.major &&
				this.minor == other.minor &&
				this.patch == other.patch;
		}

		public override bool Equals(object obj)
		{
			var other = obj as ModuleVersion;
			if (other != null)
				return this.Equals(other);
			return false;
		}

		public override int GetHashCode()
		{
			// Version numbers are probably pretty small, so we can just OR
			// some lower-order bits together.

			int hashCode = 0;

			hashCode |= (major & 0x000000FF) << 16;
			hashCode |= (minor & 0x000000FF) << 8;
			hashCode |= patch & 0x000000FF;

			return hashCode;
		}

		public override string ToString()
		{
			return string.Format(
				"{0}.{1}.{2}",
				major.ToStringInvariant(),
				minor.ToStringInvariant(),
				patch.ToStringInvariant()
			);
		}

		public ModuleVersion Parse(string value)
		{
			ModuleVersion result;
			switch (TryParseValue(value, out result))
			{
				case ParseResult.Ok:
					return result;
				case ParseResult.MissingComponent:
					throw new ArgumentException("The version string is missing one or more components", "value");
				case ParseResult.TooManyComponents:
					throw new ArgumentException("The version string has more than three components", "value");
				case ParseResult.InvalidComponent:
					throw new ArgumentException("The version string has an invalid component", "value");
				default:
					throw new ArgumentException("Unspecified parse error", "value");
			}
		}

		public bool TryParse(string value, out ModuleVersion result)
		{
			switch (TryParseValue(value, out result))
			{
				case ParseResult.Ok:
					return true;
				case ParseResult.MissingComponent:
				case ParseResult.TooManyComponents:
				case ParseResult.InvalidComponent:
				default:
					return false;
			}
		}

		private ParseResult TryParseValue(string value, out ModuleVersion result)
		{
			result = null;
			int major = -1,
				minor = -1,
				patch = -1;

			var components = value.Split(Separators);
			if (components.Length != 3)
				return components.Length < 3 ? ParseResult.MissingComponent : ParseResult.TooManyComponents;

			if (!TryParseComponent(components[0], out major))
				return ParseResult.InvalidComponent;
			if (!TryParseComponent(components[1], out minor))
				return ParseResult.InvalidComponent;
			if (!TryParseComponent(components[2], out patch))
				return ParseResult.InvalidComponent;

			result = new ModuleVersion(major, minor, patch);
			return ParseResult.Ok;
		}

		private bool TryParseComponent(string component, out int result)
		{
			return int.TryParse(component, NumberStyles.None, CI.InvariantCulture, out result);
		}

		private static readonly char[] Separators = { '.' };

		public static readonly ModuleVersion Zero = new ModuleVersion(0, 0, 0);

		public static bool operator ==(ModuleVersion a, ModuleVersion b)
		{
			return a.Equals(b);
		}

		public static bool operator !=(ModuleVersion a, ModuleVersion b)
		{
			return !a.Equals(b);
		}

		public static bool operator <(ModuleVersion a, ModuleVersion b)
		{
			return a.CompareTo(b) < 0;
		}

		public static bool operator <=(ModuleVersion a, ModuleVersion b)
		{
			return a.CompareTo(b) <= 0;
		}

		public static bool operator >(ModuleVersion a, ModuleVersion b)
		{
			return a.CompareTo(b) > 0;
		}

		public static bool operator >=(ModuleVersion a, ModuleVersion b)
		{
			return a.CompareTo(b) >= 0;
		}

		private enum ParseResult
		{
			Ok = 0,
			MissingComponent = 1,
			TooManyComponents = 2,
			InvalidComponent = 3,
		}
	}
}
