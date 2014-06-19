using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Osprey
{
	[DebuggerDisplay("Count = {Count}")]
	internal class MemberTable<T> : IEnumerable<KeyValuePair<uint, T>>
		where T : class
	{
		public MemberTable(uint mask)
			: this(mask, 0)
		{ }
		public MemberTable(uint mask, int capacity)
		{
			if ((mask & ~Module.MaskMask) != 0 || (mask & Module.MaskMask) == 0)
				throw new ArgumentException("Invalid MemberTable mask.", "mask");

			this.mask = mask;
			Initialize(capacity);
		}

		private bool readOnly = false;
		private uint mask;
		private int version;
		private int length;
		private int[] buckets;
		private Entry[] entries;

		private struct Entry
		{
			// Lower 31 bits of hash code
			internal int HashCode;
			// Index of next entry in this bucket, or -1 if none
			internal int Next;
			internal T Value;
		}

		/// <summary>
		/// Gets an entry by ID.
		/// </summary>
		/// <param name="id">The ID of the entry to retrieve.</param>
		/// <returns>The entry with the specified ID.</returns>
		/// <exception cref="ArgumentException">The mask of <paramref name="id"/> does not match the mask of the entries in the table.
		/// -or-
		/// <paramref name="id"/> refers to an entry that is not in the table.</exception>
		public T this[uint id]
		{
			get
			{
				if ((id & Module.MaskMask) != mask)
					throw new ArgumentException("Invalid member mask.", "id");

				uint value = id & ~Module.MaskMask;
				if (value < 1 || value > length)
					throw new ArgumentException("There is no entry with the specified ID.", "id");

				return entries[unchecked((int)(value - 1))].Value;
			}
		}

		/// <summary>
		/// Gets the actual number of entries in the table.
		/// </summary>
		public int Count { get { return length; } }

		/// <summary>
		/// Gets a value indicating whether the member table is readonly.
		/// </summary>
		public bool ReadOnly { get { return readOnly; } }

		private void Initialize(int capacity)
		{
			capacity = GetPrime(capacity);

			buckets = new int[capacity];
			for (var i = 0; i < capacity; i++)
				buckets[i] = -1;

			entries = new Entry[capacity];
		}

		private void Resize()
		{
			var newCapacity = GetPrime(entries.Length * 2);

			var newBuckets = new int[newCapacity];
			for (var i = 0; i < newCapacity; i++)
				newBuckets[i] = -1;

			var newEntries = new Entry[newCapacity];
			Array.Copy(entries, 0, newEntries, 0, length);

			for (var i = 0; i < length; i++)
			{
				var bucket = newEntries[i].HashCode % newCapacity;
				newEntries[i].Next = newBuckets[bucket];
				newBuckets[bucket] = i;
			}

			buckets = newBuckets;
			entries = newEntries;
		}

		private int FindEntry(T entry, out int hashCode)
		{
			hashCode = entry == null ? 0 : entry.GetHashCode() & 0x7FFFFFFF;

			for (var i = buckets[hashCode % buckets.Length]; i >= 0; i = entries[i].Next)
				if (entries[i].HashCode == hashCode && object.Equals(entry, entries[i].Value))
					return i;

			return -1;
		}

		/// <summary>
		/// Returns the ID of an entry in the table, adding it to the end if it does not exist.
		/// </summary>
		/// <param name="entry">The entry to get an ID for.</param>
		/// <returns>The ID of the entry.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="entry"/> is null.</exception>
		public uint GetId(T entry)
		{
			if (entry == null)
				throw new ArgumentNullException("entry");

			int hashCode;
			var index = FindEntry(entry, out hashCode);
			if (index >= 0)
				return mask | unchecked((uint)index + 1);

			return AddInternal(entry, hashCode);
		}

		/// <summary>
		/// Adds an entry to the table, and returns its ID.
		/// </summary>
		/// <param name="entry">The entry to add to the table.</param>
		/// <returns>The ID of the entry that was added.</returns>
		/// <exception cref="ArgumentNullException"><paramref name="entry"/> is null.</exception>
		/// <exception cref="ArgumentException"><paramref name="entry"/> is already in the table.</exception>
		public uint Add(T entry)
		{
			if (entry == null)
				throw new ArgumentNullException("entry");

			int hashCode;
			var index = FindEntry(entry, out hashCode);
			if (index >= 0)
				throw new ArgumentException("The specified entry is already in the table.", "entry");

			return AddInternal(entry, hashCode);
		}

		private uint AddInternal(T entry, int hashCode)
		{
			if (readOnly)
				throw new InvalidOperationException("Cannot modify a readonly member table.");

			if (length == entries.Length)
				Resize();

			var bucket = hashCode % buckets.Length;
			var index = length;
			entries[index].Next = buckets[bucket];
			entries[index].HashCode = hashCode;
			entries[index].Value = entry;
			buckets[bucket] = index;

			length++;
			version++;

			return mask | unchecked((uint)length);
		}

		public bool ContainsId(uint id)
		{
			var index = id & ~Module.MaskMask;
			return index >= 1 && index <= length;
		}

		public bool ContainsEntry(T entry)
		{
			if (entry == null)
				throw new ArgumentNullException("entry");

			int hashCode;
			return FindEntry(entry, out hashCode) >= 0;
		}

		/// <summary>
		/// Adds <paramref name="count"/> number of empty (null) entries.
		/// </summary>
		/// <param name="count">The number of empty entries to add.</param>
		internal void AddEmpty(int count)
		{
			for (var i = 0; i < count; i++)
				AddInternal(null, 0);
		}

		/// <summary>
		/// Locks the member table, marking it as readonly and preventing any further modifications to it.
		/// This operation is irreversible.
		/// </summary>
		internal void Lock()
		{
			readOnly = true;
		}

		private IEnumerable<KeyValuePair<uint, T>> EnumerateEntries()
		{
			var startVersion = version;
			var count = length;
			for (var i = 0; i < count; i++)
			{
				if (version != startVersion)
					throw new InvalidOperationException("The table has been modified since the enumeration began.");

				yield return new KeyValuePair<uint, T>(mask | unchecked((uint)i + 1), entries[i].Value);
			}
		}

		public IEnumerator<KeyValuePair<uint, T>> GetEnumerator()
		{
			return EnumerateEntries().GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return EnumerateEntries().GetEnumerator();
		}

		// Primes and helper methods taken from Ovum
		private static readonly int[] Primes = {
			3, 7, 11, 17, 23, 29, 37, 47, 59, 71, 89, 107, 131, 163, 197,
			239, 293, 353, 431, 521, 631, 761, 919, 1103, 1327, 1597, 1931,
			2333, 2801, 3371, 4049, 4861, 5839, 7013, 8419, 10103, 12143,
			14591, 17519, 21023, 25229, 30293, 36353, 43627, 52361, 62851,
			75431, 90523, 108631, 130363, 156437, 187751, 225307, 270371,
			324449, 389357, 467237, 560689, 672827, 807403, 968897, 1162687,
			1395263, 1674319, 2009191, 2411033, 2893249, 3471899, 4166287,
			4999559, 5999471, 7199369
		};

		private static int GetPrime(int min)
		{
			// Check the table first
			for (var i = 0; i < Primes.Length; i++)
				if (Primes[i] >= min)
					return Primes[i];

			// Outside of the table; time to compute!
			for (var i = min | 1; i < int.MaxValue; i += 2)
				if (IsPrime(i))
					return i;

			// Oh well.
			return min;
		}

		private static bool IsPrime(int n)
		{
			if ((n & 1) == 0)
				// 2 is the only even prime!
				return n == 2;

			var max = (int)Math.Sqrt((double)n);
			for (var div = 3; div <= max; div += 2)
				if ((n % div) == 0)
					return false;

			return true;
		}
	}
}