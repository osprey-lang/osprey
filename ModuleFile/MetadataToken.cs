using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public struct MetadataToken : Raw.IFixedSizeObject, IEquatable<MetadataToken>
	{
		public MetadataToken(uint value)
		{
			this.Value = value;
		}

		public MetadataToken(uint index, Raw.MemberKind memberKind)
		{
			Value = unchecked((uint)memberKind | (index + 1));
		}

		public uint Value;

		/// <summary>
		/// Gets the index value of the token, as a zero-based array index.
		/// </summary>
		public uint Index
		{
			get
			{
				return unchecked((Value & IndexMask) - 1);
			}
		}

		public Raw.MemberKind MemberKind
		{
			get
			{
				return unchecked((Raw.MemberKind)(Value & MemberKindMask));
			}
		}

		public override bool Equals(object obj)
		{
			if (obj is MetadataToken)
				return Equals((MetadataToken)obj);
			return false;
		}

		public bool Equals(MetadataToken other)
		{
			return this.Value == other.Value;
		}

		public override int GetHashCode()
		{
			return Value.GetHashCode();
		}

		public static bool operator ==(MetadataToken a, MetadataToken b)
		{
			return a.Equals(b);
		}

		public static bool operator !=(MetadataToken a, MetadataToken b)
		{
			return !a.Equals(b);
		}

		public static readonly MetadataToken Null = new MetadataToken();

		public const uint MemberKindMask = 0xff000000;
		public const uint IndexMask = 0x00ffffff;
	}
}
