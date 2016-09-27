using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Nodes;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

namespace Osprey
{
	public sealed partial class Module
	{
		// This list is only populated while opening the module.
		private List<UnresolvedConstant> unresolvedConstants = null;
		private void AddUnresolvedConstant(ImportedClassConstant member, uint typeId, long value)
		{
			if (unresolvedConstants == null)
				unresolvedConstants = new List<UnresolvedConstant>();

			unresolvedConstants.Add(new UnresolvedConstant
			{
				Member = member,
				TypeId = typeId,
				Value = value
			});
		}

		/// <summary>
		/// Loads a module from a given file.
		/// </summary>
		/// <param name="pool">The pool that the module is being opened for.</param>
		/// <param name="fileName">The name of the file to open.</param>
		/// <returns>The module that was loaded.</returns>
		internal static Module Open(ModulePool pool, string fileName, Version requiredVersion, bool fromVersionedFile)
		{
			if (fileName == null)
				throw new ArgumentNullException("fileName");

			fileName = Path.GetFullPath(fileName);

			throw new NotImplementedException();
		}

		private static ConstantValueType GetConstantType(ModuleReader reader, Module module, uint typeId, out Type type)
		{
			bool success;
			var result = GetConstantType(reader, module, typeId, out type, out success);

			if (!success)
				throw new ModuleLoadException(reader.FileName, "Could not resolve constant type ID.");

			return result;
		}

		private static ConstantValueType GetConstantType(ModuleReader reader, Module module, uint typeId, out Type type, out bool success)
		{
			var typeMask = typeId & MaskMask;
			if (typeMask == TypeRefMask)
			{
				if (!module.members.TypeRefs.ContainsId(typeId))
					throw new ModuleLoadException(reader.FileName, "Could not resolve TypeRef token.");
				type = module.members.TypeRefs[typeId];
			}
			else if (typeMask == TypeDefMask)
			{
				if (!module.members.TypeDefs.ContainsId(typeId))
				{
					type = null;
					success = false;
					return 0;
				}
				type = module.members.TypeDefs[typeId];
			}
			else
			{
				throw new ModuleLoadException(reader.FileName, "Invalid token kind: must be a TypeDef or TypeRef.");
			}

			ConstantValueType output;
			switch (type.FullName)
			{
				case StandardNames.BooleanName:
					output = ConstantValueType.Boolean;
					break;
				case StandardNames.IntName:
					output = ConstantValueType.Int;
					break;
				case StandardNames.UIntName:
					output = ConstantValueType.UInt;
					break;
				case StandardNames.RealName:
					output = ConstantValueType.Real;
					break;
				case StandardNames.StringName:
					output = ConstantValueType.String;
					break;
				default:
					{
						var baseName = type.BaseType == null ? null : type.BaseType.FullName;
						if (baseName == StandardNames.EnumName || baseName == StandardNames.EnumSetName)
							output = ConstantValueType.Enum;
						else
							throw new ModuleLoadException(reader.FileName, "Invalid constant value type.");
					}
					break;
			}

			success = true;
			return output;
		}

		private static ConstantValue ConstantValueFromRaw(ConstantValueType constantType, Module module, Type type, long value)
		{
			ConstantValue constValue;
			switch (constantType)
			{
				case ConstantValueType.Boolean:
					constValue = ConstantValue.CreateBoolean(value != 0);
					break;
				case ConstantValueType.Int:
					constValue = ConstantValue.CreateInt(value);
					break;
				case ConstantValueType.UInt:
					constValue = ConstantValue.CreateUInt(unchecked((ulong)value));
					break;
				case ConstantValueType.Real:
					constValue = ConstantValue.CreateReal(BitConverter.Int64BitsToDouble(value));
					break;
				case ConstantValueType.String:
					{
						uint stringId = unchecked((uint)value);
						constValue = ConstantValue.CreateString(module.members.Strings[stringId]);
					}
					break;
				case ConstantValueType.Enum:
					constValue = ConstantValue.CreateEnumValue(value, (Enum)type);
					break;
				default: throw new InvalidOperationException("This isn't supposed to happen.");
			}
			return constValue;
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, TypeFlags flags)
		{
			if ((flags & TypeFlags.Public) == TypeFlags.Public)
				return AccessLevel.Public;
			if ((flags & TypeFlags.Private) == TypeFlags.Private)
				return AccessLevel.Private;

			throw new ModuleLoadException(reader.FileName, "TypeDef has no declared accessibility.");
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, FieldFlags flags)
		{
			if ((flags & FieldFlags.Public) == FieldFlags.Public)
				return AccessLevel.Public;
			if ((flags & FieldFlags.Private) == FieldFlags.Private)
				return AccessLevel.Private;
			if ((flags & FieldFlags.Protected) == FieldFlags.Protected)
				return AccessLevel.Protected;

			throw new ModuleLoadException(reader.FileName, "FieldDef has no declared accessibility.");
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, MethodFlags flags)
		{
			if ((flags & MethodFlags.Public) == MethodFlags.Public)
				return AccessLevel.Public;
			if ((flags & MethodFlags.Private) == MethodFlags.Private)
				return AccessLevel.Private;
			if ((flags & MethodFlags.Protected) == MethodFlags.Protected)
				return AccessLevel.Protected;

			throw new ModuleLoadException(reader.FileName, "MethodDef or FunctionDef has no declared accessibility.");
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, ConstantFlags flags)
		{
			if ((flags & ConstantFlags.Public) == ConstantFlags.Public)
				return AccessLevel.Public;
			if ((flags & ConstantFlags.Private) == ConstantFlags.Private)
				return AccessLevel.Private;

			throw new ModuleLoadException(reader.FileName, "ConstantDef has no declared accessibility.");
		}

		private static string[] GetPathFromName(string fullName, out string name)
		{
			var nameParts = fullName.Split(Compiler.Dot);
			var lastIndex = nameParts.Length - 1;
			var result = new string[lastIndex];

			for (var i = 0; i < lastIndex; i++)
				result[i] = nameParts[i];

			name = nameParts[lastIndex];
			return result;
		}

		private struct UnresolvedConstant
		{
			public ImportedClassConstant Member;
			public uint TypeId;
			public long Value;
		}
	}
}