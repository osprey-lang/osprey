using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Osprey.Json
{
	// This file contains a very simple, straightforward JSON implementation thing.
	// This compiler does NOT contain a JSON parser, only a JSON generator. For that
	// reason, all of the types defined here are internal.

	internal abstract class JsonValue
	{
		protected JsonValue(JsonType type)
		{
			this.type = type;
		}

		private JsonType type;
		public JsonType Type { get { return type; } }

		public override string ToString()
		{
#if DEBUG
			return ToString(true);
#else
			return ToString(false);
#endif
		}

		public string ToString(bool prettyPrint)
		{
			var sb = new StringBuilder();

			this.ToString(sb, prettyPrint ? 0 : -1);

			return sb.ToString();
		}

		public abstract void ToString(StringBuilder target, int indent);

		public static void ValueToString(JsonValue value, StringBuilder target, int indent)
		{
			if (value == null)
				target.Append("null");
			else
				value.ToString(target, indent);
		}
	}

	internal enum JsonType
	{
		Boolean,
		Number,
		String,
		Array,
		Object,
	}

	internal sealed class JsonBoolean : JsonValue
	{
		public JsonBoolean(bool value)
			: base(JsonType.Boolean)
		{
			this.value = value;
		}

		private bool value;
		public bool Value { get { return value; } }

		public override void ToString(StringBuilder target, int indent)
		{
			target.Append(value ? "true" : "false");
		}

		public static bool operator true(JsonBoolean value)
		{
			return value.value;
		}

		public static bool operator false(JsonBoolean value)
		{
			return !value.value;
		}

		public static implicit operator bool(JsonBoolean value)
		{
			if (value == null)
				throw new ArgumentNullException("value");
			return value.value;
		}

		public static explicit operator JsonBoolean(bool value)
		{
			return new JsonBoolean(value);
		}
	}

	internal sealed class JsonNumber : JsonValue
	{
		public JsonNumber(double value)
			: base(JsonType.Number)
		{
			if (double.IsNaN(value) || double.IsInfinity(value))
				throw new ArgumentException("A JSON number cannot be infinity or NaN.", "value");
			this.value = value;
		}

		private double value;
		public double Value { get { return value; } }

		public override void ToString(StringBuilder target, int indent)
		{
			target.Append(value.ToJsonString());
		}

		public static implicit operator double(JsonNumber value)
		{
			if (value == null)
				throw new ArgumentNullException("value");
			return value.value;
		}

		public static explicit operator JsonNumber(double value)
		{
			return new JsonNumber(value);
		}
	}

	internal sealed class JsonString : JsonValue
	{
		public JsonString(string value)
			: base(JsonType.String)
		{
			if (value == null)
				throw new ArgumentNullException("value");

			this.value = value;
		}

		private string value;
		public string Value { get { return value; } }

		public override void ToString(StringBuilder target, int indent)
		{
			target.Append(value.ToJsonString());
		}

		public static implicit operator string(JsonString value)
		{
			if (value == null)
				throw new ArgumentNullException("value");
			return value.value;
		}

		public static explicit operator JsonString(string value)
		{
			return new JsonString(value);
		}
	}

	internal class JsonArray : JsonValue, IList<JsonValue>
	{
		public JsonArray()
			: base(JsonType.Array)
		{
			this.values = new List<JsonValue>();
		}
		public JsonArray(int capacity)
			: base(JsonType.Array)
		{
			this.values = new List<JsonValue>(capacity);
		}
		public JsonArray(params JsonValue[] values)
			: base(JsonType.Array)
		{
			this.values = new List<JsonValue>(values);
		}
		public JsonArray(IEnumerable<JsonValue> values)
			: base(JsonType.Array)
		{
			this.values = new List<JsonValue>(values);
		}

		private List<JsonValue> values;

		public JsonValue this[int index]
		{
			get { return values[index]; }
			set { values[index] = value; }
		}

		public int Count { get { return values.Count; } }

		public bool IsReadOnly { get { return false; } }

		public void Add(JsonValue item)
		{
			values.Add(item);
		}

		public void Insert(int index, JsonValue item)
		{
			values.Insert(index, item);
		}

		public bool Contains(JsonValue item)
		{
			return values.Contains(item);
		}

		public int IndexOf(JsonValue item)
		{
			return values.IndexOf(item);
		}

		public void RemoveAt(int index)
		{
			values.RemoveAt(index);
		}

		public bool Remove(JsonValue item)
		{
			return values.Remove(item);
		}

		public void Clear()
		{
			values.Clear();
		}

		public void CopyTo(JsonValue[] array, int arrayIndex)
		{
			values.CopyTo(array, arrayIndex);
		}

		public List<JsonValue>.Enumerator GetEnumerator()
		{
			return values.GetEnumerator();
		}

		public override void ToString(StringBuilder target, int indent)
		{
			if (indent < 0)
				target.Append('[');
			else
				target.AppendLine("[");

			for (var i = 0; i < values.Count; i++)
			{
				if (i > 0)
					if (indent < 0)
						target.Append(',');
					else
						target.AppendLine(",");

				if (indent >= 0)
				{
					target.Append('\t', indent + 1);
					ValueToString(values[i], target, indent + 1);
				}
				else
					ValueToString(values[i], target, -1);
			}

			if (indent >= 0)
			{
				target.AppendLine();
				target.Append('\t', indent);
			}
			target.Append(']');
		}

		IEnumerator<JsonValue> IEnumerable<JsonValue>.GetEnumerator()
		{
			return values.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return values.GetEnumerator();
		}
	}

	internal class JsonObject : JsonValue, IDictionary<string, JsonValue>
	{
		public JsonObject()
			: base(JsonType.Object)
		{
			this.members = new Dictionary<string, JsonValue>();
		}
		public JsonObject(int capacity)
			: base(JsonType.Object)
		{
			this.members = new Dictionary<string, JsonValue>(capacity);
		}
		public JsonObject(IDictionary<string, JsonValue> values)
			: base(JsonType.Object)
		{
			this.members = new Dictionary<string, JsonValue>(values);
		}

		public JsonObject(IDictionary<string, string> values)
			: base(JsonType.Object)
		{
			members = new Dictionary<string, JsonValue>(values.Count);
			foreach (var kvp in values)
				members.Add(kvp.Key, (JsonString)kvp.Value);
		}

		private Dictionary<string, JsonValue> members;

		public JsonValue this[string key]
		{
			get { return members[key]; }
			set { members[key] = value; }
		}

		public int Count { get { return members.Count; } }

		public bool IsReadOnly { get { return false; } }

		public ICollection<string> Keys { get { return members.Keys; } }
		public ICollection<JsonValue> Values { get { return members.Values; } }

		public void Add(KeyValuePair<string, JsonValue> item)
		{
			members.Add(item.Key, item.Value);
		}

		public void Add(string key, JsonValue value)
		{
			members.Add(key, value);
		}

		public bool ContainsKey(string key)
		{
			return members.ContainsKey(key);
		}

		public bool Contains(KeyValuePair<string, JsonValue> item)
		{
			return members.ContainsKey(item.Key) && members[item.Key].Equals(item.Value);
		}

		public bool Remove(string key)
		{
			return members.Remove(key);
		}

		public bool Remove(KeyValuePair<string, JsonValue> item)
		{
			throw new NotSupportedException();
		}

		public void Clear()
		{
			members.Clear();
		}

		public bool TryGetValue(string key, out JsonValue value)
		{
			return members.TryGetValue(key, out value);
		}

		public void CopyTo(KeyValuePair<string, JsonValue>[] array, int arrayIndex)
		{
			throw new NotSupportedException();
		}

		public Dictionary<string, JsonValue>.Enumerator GetEnumerator()
		{
			return members.GetEnumerator();
		}

		public override void ToString(StringBuilder target, int indent)
		{
			if (indent < 0)
				target.Append('{');
			else
				target.AppendLine("{");

			var needSep = false;
			foreach (var kvp in members)
			{
				if (needSep)
					if (indent < 0)
						target.Append(',');
					else
						target.AppendLine(",");
				else
					needSep = true;

				if (indent >= 0)
				{
					target.Append('\t', indent + 1);
					target.Append(kvp.Key.ToJsonString());
					target.Append(": ");
					ValueToString(kvp.Value, target, indent + 1);
				}
				else
				{
					target.Append(kvp.Key.ToJsonString());
					target.Append(':');
					ValueToString(kvp.Value, target, -1);
				}
			}

			if (indent >= 0)
			{
				target.AppendLine();
				target.Append('\t', indent);
			}
			target.Append('}');
		}

		IEnumerator<KeyValuePair<string, JsonValue>> IEnumerable<KeyValuePair<string, JsonValue>>.GetEnumerator()
		{
			return members.GetEnumerator();
		}

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return members.GetEnumerator();
		}
	}
}