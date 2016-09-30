using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	/*
	 * For details on the design and layout of FileObjects, see the associated file
	 * ModuleFormatNotes.md.
	 */

	public abstract class FileObject
	{
		public uint Address
		{
			get
			{
				if (layoutParent != null)
					return layoutParent.Address + relativeAddress;
				return relativeAddress;
			}
		}

		private uint relativeAddress;
		public uint RelativeAddress
		{
			get { return relativeAddress; }
			set
			{
				if (value % Alignment != 0)
					throw new ArgumentException("Incorrectly aligned address.", "value");
				relativeAddress = value;
			}
		}

		private FileObject layoutParent;
		public FileObject LayoutParent
		{
			get { return layoutParent; }
			internal set { layoutParent = value; }
		}

		public abstract uint Size { get; }

		public abstract uint Alignment { get; }

		public uint AlignedSize
		{
			get { return AlignTo(Size, Alignment); }
		}

		/// <summary>
		/// When overridden in a derived class, lays out any <see cref="FileObject"/> children
		/// that this object contains. This method is called once, to finalize the addresses of
		/// all file objects.
		/// </summary>
		public virtual void LayOutChildren()
		{ }

		protected static uint AlignTo(uint value, uint alignment)
		{
			// This effectively does the same as
			//   Math.Ceiling(size / alignment) * alignment
			// but using only integer arithmetics.
			return (value + alignment - 1) / alignment * alignment;
		}

		protected static uint LayOutItems(uint startOffset, IEnumerable<FileObject> items)
		{
			foreach (var item in items)
			{
				item.RelativeAddress = startOffset;
				item.LayOutChildren();
				startOffset += item.AlignedSize;
			}
			return startOffset;
		}

		public Raw.Rva<T> ToRva<T>()
			where T : struct
		{
			return new Raw.Rva<T>(Address);
		}

		public abstract void Emit(MemoryMappedViewAccessor view);
	}

	public abstract class FileSection : FileObject
	{
		public abstract override void LayOutChildren();
	}

	// This class can safely be used with variable-size file objects, as long as they have
	// the same alignment. The alignment will be reported as the alignment of the first item,
	// or 1 if the array is empty.
	[DebuggerDisplay("Count = {Count}")]
	public class FileObjectArray<T> : FileObject, IEnumerable<T>
		where T : FileObject
	{
		public FileObjectArray(FileObject layoutParent)
			: this(layoutParent, 0)
		{ }

		public FileObjectArray(FileObject layoutParent, int capacity)
		{
			this.LayoutParent = layoutParent;
			items = new TempList<T>(capacity);
		}

		public T this[int index] { get { return items[index]; } }

		public int Count { get { return items.Count; } }

		public override uint Size
		{
			get
			{
				if (items.Count == 0)
					return 0;
				var lastItem = items[items.Count - 1];
				return lastItem.RelativeAddress + lastItem.AlignedSize;
			}
		}

		public override uint Alignment
		{
			get
			{
				if (items.Count == 0)
					return 1; // 1 to prevent division-by-zero errors
				return items[0].Alignment;
			}
		}

		private TempList<T> items;

		public void Add(T item)
		{
			if (item == null)
				throw new ArgumentNullException("item");

			item.LayoutParent = this;
			// Possibly temporary address; LayOutChildren may change it.
			// this.Size is last item + aligned size, which is exactly what we want.
			item.RelativeAddress = this.Size;

			items.Add(item);
		}

		public override void LayOutChildren()
		{
			LayOutItems(0, items);
		}

		public new Raw.RvaToArray<TRva> ToRva<TRva>()
			where TRva : struct
		{
			if (Count == 0)
				return Raw.RvaToArray<TRva>.Null;
			else
				return new Raw.RvaToArray<TRva>(Address);
		}

		public Raw.RvaToArray<TRva> ToRva<TRva>(out int countField)
			where TRva : struct
		{
			countField = this.Count;
			return ToRva<TRva>();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			if (items.Count == 0)
				return;

			foreach (var item in items)
				item.Emit(view);
		}

		public IEnumerator<T> GetEnumerator()
		{
			return items.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}
	}

	// This class resembles FileObjectArray<T>, except that it can safely be used with
	// objects of /different/ alignment. Everything within the array will be aligned
	// according to its own alignment. As the name implies, this class is only really
	// used for file sections, but note that this includes sections within sections,
	// such as the ClassDefs, MethodDefs, FieldDefs, etc. inside the DefinitionsSection.
	public class FileSectionArray<T> : FileObject, IEnumerable<T>
		where T : FileObject
	{
		public FileSectionArray(FileObject layoutParent, int capacity)
		{
			this.LayoutParent = layoutParent;
			this.items = new TempList<T>(capacity);
		}

		public T this[int index] { get { return items[index]; } }

		public int Count { get { return items.Count; } }

		public override uint Size
		{
			get
			{
				if (items.Count == 0)
					return 0;
				var lastItem = items[items.Count - 1];
				// Don't add AlignedSize here - the next item may not be aligned the same way,
				// so it makes no difference anyway. Plus, we /don't/ want the final size of
				// the module to be dependent on the last section's alignment.
				return lastItem.RelativeAddress + lastItem.Size;
			}
		}

		public override uint Alignment
		{
			get
			{
				// We can safely return 1 here, because every section will be aligned according
				// to its own alignment. Hence it doesn't matter where we put this array; it'll
				// shift to fit its contents.
				return 1;
			}
		}

		private TempList<T> items;

		public void Add(T item)
		{
			if (item == null)
				throw new ArgumentNullException("item");

			item.LayoutParent = this;
			// Possibly temporary address; LayOutChildren may change it.
			item.RelativeAddress = AlignTo(this.Size, item.Alignment);

			items.Add(item);
		}

		public override void LayOutChildren()
		{
			// Can't call LayOutItems here - it assumes every item has the same alignment.
			uint startOffset = 0u;
			foreach (var item in items)
			{
				startOffset = AlignTo(startOffset, item.Alignment);
				item.RelativeAddress = startOffset;
				item.LayOutChildren();
				startOffset += item.AlignedSize;
			}
		}

		public new Raw.RvaToArray<TRva> ToRva<TRva>()
			where TRva : struct
		{
			if (Count == 0)
				return Raw.RvaToArray<TRva>.Null;
			else
				return new Raw.RvaToArray<TRva>(Address);
		}

		public Raw.RvaToArray<TRva> ToRva<TRva>(out int countField)
			where TRva : struct
		{
			countField = this.Count;
			return ToRva<TRva>();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			if (items.Count == 0)
				return;

			foreach (var item in items)
				item.Emit(view);
		}

		public IEnumerator<T> GetEnumerator()
		{
			return items.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}
	}
}
