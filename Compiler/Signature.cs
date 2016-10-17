using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Json;
using Osprey.Nodes;

namespace Osprey
{
	/// <summary>
	/// Represents the signature of a parametrized member.
	/// </summary>
	public struct Signature
	{
		public Signature(IEnumerable<Parameter> parameters, Splat splat)
		{
			if (splat == Splat.None)
			{
				var paramCount = 0;
				var optionalParamCount = 0;
				foreach (var param in parameters)
				{
					paramCount++;
					if (param.DefaultValue != null)
						optionalParamCount++;
				}
				this.paramCount = paramCount;
				this.optionalParamCount = optionalParamCount;
			}
			else
			{
				this.paramCount = parameters.Count();
				this.optionalParamCount = 1; // last parameter is always optional in a variadic overload
			}

			this.splat = splat;
		}
		public Signature(int paramc, int optionalParamc, Splat splat)
		{
			this.paramCount = paramc;
			this.optionalParamCount = splat == Splat.None ? optionalParamc : 1;
			this.splat = splat;
		}

		private int paramCount, optionalParamCount;
		/// <summary>Gets the number of parameters the member takes.</summary>
		public int ParameterCount { get { return paramCount; } }
		/// <summary>Gets the number of optional parameters the member has.</summary>
		public int OptionalParameterCount { get { return optionalParamCount; } }

		private Splat splat;
		/// <summary>Gets the location of the splat, if any.</summary>
		public Splat Splat { get { return splat; } }

		public override int GetHashCode()
		{
			return (int)((uint)paramCount & 0x0000ffff | (((uint)optionalParamCount & 0xffff0000) << 16)) ^ (int)splat;
		}
		public override bool Equals(object obj)
		{
			if (obj is Signature)
				return this == (Signature)obj;
			return base.Equals(obj);
		}

		/// <summary>
		/// Determines whether the signature is ambiguous with another.
		/// </summary>
		/// <param name="other">The other signature.</param>
		/// <returns>True if the signatures are ambiguous; otherwise, false.</returns>
		public bool IsAmbiguous(Signature other)
		{
			return AreAmbiguous(this, other);
		}

		/// <summary>
		/// Gets the overlap of this signature with another.
		/// </summary>
		/// <param name="other">The other signature.</param>
		/// <returns>The overlap of this signature with another.</returns>
		public SignatureOverlap GetOverlap(Signature other)
		{
			return Signature.GetOverlap(this, other);
		}

        /// <summary>
        /// Determines whether the method accepts the specified number of arguments.
        /// </summary>
        /// <param name="argCount">The number of arguments to test against.</param>
        /// <returns>True if the method can be invoked with <paramref name="argCount"/> arguments; otherwise, false.</returns>
        public bool Accepts(int argCount)
        {
            if (splat != Splat.None)
                return argCount >= paramCount - 1;
            else
                return argCount >= paramCount - optionalParamCount && argCount <= paramCount;
        }

		internal JsonObject ToJson()
		{
			var output = new JsonObject();

			output.Add("minArgs", new JsonNumber(paramCount - optionalParamCount));
			if (splat == Splat.None)
				output.Add("maxArgs", new JsonNumber(paramCount));

			return output;
		}

		private static bool AreAmbiguous(Signature a, Signature b)
		{
			if (a.splat != Splat.None && b.splat != Splat.None)
				return true;

			// If a is variadic, then b must not have more than a.paramCount - 1 required params.
			// E.g.:
			//    foo(a, b, c, ...d)  paramCount = 4
			//    foo(a, b)           OK    (2 >= 3 == false)
			//    foo()               OK    (0 >= 3 == false)
			//    foo(a, b, c)        Error (3 >= 3 == true)
			//    foo(a, b, c=0, d=0) Error (5 >= 3 == true)
			if (a.splat != Splat.None)
				return b.paramCount >= a.paramCount - 1;
			// If b is variadic, then the reverse is true.
			if (b.splat != Splat.None)
				return a.paramCount >= b.paramCount - 1;

			// Otherwise, we calculate a range of possible values for each
			// signature, and see if those ranges overlap at all.
			// 0     1     2     3     4     5
			//       +-----------+               foo(a, b=0, c=0)
			//      amin        a.paramCount
			//
			//                         +-----+   foo(a, b, c, d, e=0)
			//                        bmin  b.paramCount

			int amin = a.paramCount - a.optionalParamCount;
			int bmin = b.paramCount - b.optionalParamCount;

			return amin <= bmin && bmin <= a.paramCount ||
				bmin <= amin && amin <= b.paramCount;
		}

		private static SignatureOverlap GetOverlap(Signature a, Signature b)
		{
			// Note: this relies on optionalParamCount being set to 1 if splat != Splat.None
			int amin, amax, bmin, bmax;
			amin = a.paramCount - a.optionalParamCount;
			bmin = b.paramCount - b.optionalParamCount;

			if (a.splat == Splat.None)
				amax = a.paramCount;
			else
				amax = -1;

			if (b.splat == Splat.None)
				bmax = b.paramCount;
			else
				bmax = -1;

			var min = Math.Max(amin, bmin);
			var max = amax == -1 ? bmax :
				bmax == -1 ? amax :
				Math.Min(amax, bmax);

			return new SignatureOverlap(min, max);
		}

		public static bool operator ==(Signature a, Signature b)
		{
			return a.paramCount == b.paramCount &&
				a.optionalParamCount == b.optionalParamCount &&
				a.splat == b.splat;
		}
		public static bool operator !=(Signature a, Signature b)
		{
			return !(a == b);
		}

		/// <summary>
		/// Contains a signature with no parameters and no splat.
		/// </summary>
		public static readonly Signature Empty = new Signature();
		/// <summary>
		/// Contains a signature with one required parameter and no splat.
		/// </summary>
		public static readonly Signature OneRequired = new Signature(1, 0, Splat.None);
    }

	/// <summary>
	/// Represents an overlap of two signatures, which is a range of numbers of arguments.
	/// If the method is called with N arguments and <see cref="Min"/> &lt;= N &lt;= <see cref="Max"/>,
	/// then the call is ambiguous.
	/// </summary>
	public struct SignatureOverlap
	{
		public SignatureOverlap(int min, int max)
		{
			this.min = min;
			this.max = max;
		}

		private int min, max;
		/// <summary>
		/// The minimum number of arguments (inclusive) for which the call is ambiguous. If the signatures are not ambiguous, this and <see cref="Max"/> are set max 0.
		/// </summary>
		public int Min { get { return min; } }
		/// <summary>
		/// The maximum number of arguments (inclusive) for which the call is ambiguous, or -1 if both overloads are variadic. If the signatures are not ambiguous, this and <see cref="Min"/> are set max 0.
		/// </summary>
		public int Max { get { return max; } }

		/// <summary>
		/// Gets a value indicating whether the <see cref="SignatureOverlap"/> actually has an overlap.
		/// </summary>
		public bool HasOverlap { get { return min != 0 && max != 0; } }
	}
}
