using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class ConstantValueObject : FileObject
	{
		public ConstantValueObject(ConstantValue value)
		{
			this.Value = value;
		}

		public readonly ConstantValue Value;

		public override uint Size { get { return 16; } }

		public override uint Alignment { get { return 8; } }

		public override int GetHashCode()
		{
			return Value.GetHashCode();
		}

		public override bool Equals(object obj)
		{
			var other = obj as ConstantValueObject;
			if (other != null)
				return Equals(other);
			return false;
		}

		public bool Equals(ConstantValueObject other)
		{
			return this.Value.Equals(other.Value);
		}
	}

	public class ConstantPool : FileSection
	{
		public ConstantPool()
		{
			values = new FileObjectArray<ConstantValueObject>(this, 10);
		}

		private FileObjectArray<ConstantValueObject> values;
		private Dictionary<ConstantValue, ConstantValueObject> valueMapping = new Dictionary<ConstantValue, ConstantValueObject>();

		public override uint Size { get { return values.Size; } }

		public override uint Alignment { get { return values.Alignment; } }

		public ConstantValueObject Add(ConstantValue value)
		{
			ConstantValueObject cvo;
			if (!valueMapping.TryGetValue(value, out cvo))
			{
				cvo = new ConstantValueObject(value);
				values.Add(cvo);
				valueMapping.Add(value, cvo);
			}

			return cvo;
		}

		public override void LayOutChildren()
		{
			values.LayOutChildren();
		}
	}
}
