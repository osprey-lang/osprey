using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Implements an automatically resized, indexed collection. This is conceptually
	/// equivalent to <see cref="List{T}"/>, except that it is a struct (and copying
	/// it by value is NOT safe because of a shared internal array), and it is not
	/// versioned (modifying the list while enumerating it does not throw anything).
	/// </summary>
	/// <remarks>
	/// <para>
	/// This type is primarily intended to be used for building temporary (hence "Temp")
	/// lists on the stack, which are then turned into arrays. The <see cref="ToArray()"/>
	/// method returns the internal array directly if the list's <see cref="Capacity"/>
	/// is equal to its <see cref="Count"/>.
	/// </para>
	/// <para>
	/// A <see cref="TempList{T}"/> constructed with the default constructor is safe to use.
	/// </para>
	/// <para>
	/// The instance members of this struct are not thread-safe.
	/// </para>
	/// </remarks>
	/// <typeparam name="T">The type of the items contained in the list.</typeparam>
	internal struct TempList<T> : IList<T>
	{
		public TempList(int capacity)
		{
			length = 0;
			items = new T[capacity];
		}

		public TempList(IEnumerable<T> collection)
		{
			if (collection == null)
				throw new ArgumentNullException("collection");

			var c = collection as ICollection<T>;
			if (c != null)
			{
				var count = c.Count;
				if (count == 0)
				{
					items = EmptyArray;
				}
				else
				{
					items = new T[count];
					c.CopyTo(items, 0);
				}
				length = count;
			}
			else
			{
				length = 0;
				items = EmptyArray;

				using (var e = collection.GetEnumerator())
					while (e.MoveNext())
						Add(e.Current);
			}
		}

		private int length;
		/// <summary>
		/// The items in the list. This field is null only when <see cref="length"/> is 0.
		/// </summary>
		private T[] items;

		public T this[int index]
		{
			get
			{
				if (index < 0 || index >= length)
					ErrorArgumentOutOfRange("index");
				return items[index];
			}
			set
			{
				if (index < 0 || index >= length)
					ErrorArgumentOutOfRange("index");
				items[index] = value;
			}
		}

		/// <summary>
		/// Gets the number of items contained in the list.
		/// </summary>
		public int Count { get { return length; } }

		/// <summary>
		/// Gets or sets the capacity of the list; that is,
		/// the number of items the list can contain before
		/// it needs to be resized.
		/// </summary>
		public int Capacity
		{
			get { return items == null ? 0 : items.Length; }
			set
			{
				if (value < length)
					ErrorArgumentOutOfRange("value");

				if (items == null || value != items.Length)
				{
					if (value > 0)
					{
						var newItems = new T[value];
						if (length > 0)
							// If length > 0, then 'items' cannot be null
							Array.Copy(items, 0, newItems, 0, length);
						items = newItems;
					}
					else
						items = EmptyArray;
				}
			}
		}

		private void EnsureCapacity(int min)
		{
			int cap = Capacity;
			if (cap < min)
			{
				int newCapacity = cap == 0 ? DefaultCapacity : items.Length * 2;
				if (newCapacity < min)
					newCapacity = min;
				Capacity = min;
			}
		}

		public void Add(T item)
		{
			if (items == null || length == items.Length)
				EnsureCapacity(length + 1);
			items[length++] = item;
		}

		public void AddRange(IEnumerable<T> collection)
		{
			if (collection == null)
				throw new ArgumentNullException("collection");

			var c = collection as ICollection<T>;
			if (c != null)
			{
				int count = c.Count;
				if (count > 0)
				{
					EnsureCapacity(length + count);

					var newItems = new T[count];
					c.CopyTo(newItems, 0);
					newItems.CopyTo(items, length);
					length += count;
				}
			}
			else
			{
				using (var e = collection.GetEnumerator())
					while (e.MoveNext())
						Add(e.Current);
			}
		}

		public void Insert(int index, T item)
		{
			if (index < 0 || index > length)
				ErrorArgumentOutOfRange("index");

			if (items == null || length == items.Length)
				EnsureCapacity(length + 1);

			if (index < length)
				Array.Copy(items, index, items, index + 1, length - index);
			items[index] = item;
			length++;
		}

		public int IndexOf(T item)
		{
			if (items != null)
				return Array.IndexOf(items, item, 0, length);
			return -1;
		}

		public bool Contains(T item)
		{
			return IndexOf(item) >= 0;
		}

		public void Clear()
		{
			length = 0;
			items = EmptyArray;
		}

		public bool Remove(T item)
		{
			var index = IndexOf(item);
			if (index >= 0)
			{
				RemoveAt(index);
				return true;
			}

			return false;
		}

		public void RemoveAt(int index)
		{
			if (index < 0 || index >= length)
				ErrorArgumentOutOfRange("index");

			length--;
			if (index < length)
				Array.Copy(items, index + 1, items, index, length - index);
			items[length] = default(T);
		}

		public void CopyTo(T[] array, int arrayIndex)
		{
			// Let Array.Copy check for errors
			Array.Copy(items ?? EmptyArray, 0, array, arrayIndex, length);
		}

		/// <summary>
		/// Returns an array containing the items of the list.
		/// If the list's capacity is equal to its length, this
		/// method returns the internal array for the list;
		/// otherwise, a new array is created.
		/// </summary>
		public T[] ToArray()
		{
			if (length == 0)
				return EmptyArray;
			if (items.Length == length)
				// Don't create a copy if the array is full
				return items;
			var result = new T[length];
			Array.Copy(items, 0, result, 0, length);
			return result;
		}

		/// <summary>
		/// Returns an array containing the items of the list.
		/// This method always allocates a new array.
		/// </summary>
		public T[] ToNewArray()
		{
			if (length == 0)
				return EmptyArray;
			var result = new T[length];
			Array.Copy(items, 0, result, 0, length);
			return result;
		}

		public Enumerator GetEnumerator()
		{
			return new Enumerator(items ?? EmptyArray, length);
		}

		private const int DefaultCapacity = 4;
		private static T[] EmptyArray = new T[0];

		private static void ErrorArgumentOutOfRange(string paramName)
		{
			throw new ArgumentOutOfRangeException(paramName);
		}

		bool ICollection<T>.IsReadOnly { get { return false; } }

		IEnumerator<T> IEnumerable<T>.GetEnumerator()
		{
			return GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		internal struct Enumerator : IEnumerator<T>
		{
			internal Enumerator(T[] items, int length)
			{
				this.items = items;
				this.length = length;
				this.index = 0;
				this.current = default(T);
			}

			private T[] items;
			private int length;
			private int index;
			private T current;

			public T Current { get { return current; } }

			public void Dispose() { }

			public bool MoveNext()
			{
				if (index < length)
				{
					current = items[index];
					index++;
					return true;
				}
				return false;
			}

			public void Reset()
			{
				index = 0;
			}

			object System.Collections.IEnumerator.Current { get { return current; } }
		}
	}
}