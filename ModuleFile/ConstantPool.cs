using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;

namespace Osprey.ModuleFile
{
	public class ConstantValueObject : FileObject
	{
		public ConstantValueObject(ConstantValue value, uint typeToken, uint stringToken)
		{
			this.Value = value;
			this.TypeToken = typeToken;
			this.StringToken = stringToken;
		}

		public readonly ConstantValue Value;
		public readonly uint TypeToken;
		public readonly uint StringToken;

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

		public override void Emit(MemoryMappedViewAccessor view)
		{
			var value = new Raw.ConstantValueStruct();
			switch (Value.Type)
			{
				case ConstantValueType.Null:
					value.Type = MetadataToken.Null;
					value.Value = 0UL;
					break;
				case ConstantValueType.Boolean:
				case ConstantValueType.Int:
				case ConstantValueType.UInt:
				case ConstantValueType.Real:
				case ConstantValueType.Char:
					value.Type = new MetadataToken(TypeToken);
					value.Value = Value.GetRawValue();
					break;
				case ConstantValueType.String:
					value.Type = new MetadataToken(TypeToken);
					value.StringValue = new MetadataToken(StringToken);
					break;
				case ConstantValueType.Enum:
					{
						var enumValue = Value.EnumValue;
						value.Type = new MetadataToken(enumValue.Type.Id);
						value.Value = unchecked((ulong)enumValue.Value);
					}
					break;
				default:
					throw new InvalidOperationException("Invalid ConstantValueType");
			}
			view.Write(this.Address, ref value);
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

		public ConstantValueObject Add(ConstantValue value, uint typeToken, uint stringToken)
		{
			ConstantValueObject cvo;
			if (!valueMapping.TryGetValue(value, out cvo))
			{
				cvo = new ConstantValueObject(value, typeToken, stringToken);
				values.Add(cvo);
				valueMapping.Add(value, cvo);
			}

			return cvo;
		}

		public override void LayOutChildren()
		{
			values.LayOutChildren();
		}

		public override void Emit(MemoryMappedViewAccessor view)
		{
			values.Emit(view);
		}
	}
}
