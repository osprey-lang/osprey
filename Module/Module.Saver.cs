using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;

namespace Osprey
{
	public sealed partial class Module
	{
		public void Save(string targetPath)
		{
			if (imported)
				throw new InvalidOperationException("Cannot save imported module.");

			using (var stream = File.Open(targetPath, FileMode.Create, FileAccess.Write, FileShare.None))
				Save(stream);
		}

		public void Save(Stream target)
		{
			if (imported)
				throw new InvalidOperationException("Cannot save imported module.");

			var writer = new ModuleWriter(target, Encoding.Unicode);

			writer.Write(MagicNumber, 0, 4); // Magic number
			for (var i = 0; i < DataStart - 4; i++) // Pad with zeroes until DataStart
				writer.Write((byte)0);

			writer.Write(this.name); // Module name
			writer.Write(this.version); // Module version
			WriteMetadata(writer);
			writer.Write(this.nativeLib ?? ""); // native library

			// Counts
			writer.Write(members.TypeDefs.Count); // typeCount
			writer.Write(members.GlobalFuncDefs.Count); // functionCount
			writer.Write(members.GlobalConstDefs.Count); // constantCount
			writer.Write(members.FieldDefs.Count); // fieldCount
			writer.Write(members.MethodDefs.Count); // methodCount

			// Start of method block (return to this later)
			var methodStartTarget = writer.BaseStream.Position;
			writer.Write(0u);

			WriteStrings(writer, members.Strings); // strings

			// References
			WriteModuleRefs(writer, members.ModuleRefs); // moduleRefs
			WriteTypeRefs(writer, members.TypeRefs); // typeRefs
			WriteFunctionRefs(writer, members.GlobalFuncRefs); // functionRefs
			WriteFieldRefs(writer, members.FieldRefs); // fieldRefs
			WriteMethodRefs(writer, members.MethodRefs); // methodRefs

			// Definitions
			WriteTypeDefs(writer, members.TypeDefs); // types
			WriteFunctionDefs(writer, members.GlobalFuncDefs); // functions
			WriteConstantDefs(writer, members.GlobalConstDefs); // constants

			if (mainMethod == null) // mainMethod
				writer.Write(0u);
			else
				writer.Write(mainMethod.Id);

			// Current position is now where the method block begins,
			// so go back and update methodStart
			var methodStart = writer.BaseStream.Position;

			writer.BaseStream.Seek(methodStartTarget, SeekOrigin.Begin);
			writer.Write(checked((uint)methodStart));

			writer.BaseStream.Seek(methodStart, SeekOrigin.Begin);

			writer.Write(checked((uint)methodBlock.Length)); // methodBodies.length
			methodBlock.WriteTo(target); // methodBodies
		}

		private void WriteMetadata(ModuleWriter writer)
		{
			if (metadata.Count == 0)
			{
				writer.Write(0u); // Size
			}
			else
			{
				writer.BeginCollection(metadata.Count);

				foreach (var kvp in metadata.OrderBy(kvp => kvp.Key, StringComparer.InvariantCultureIgnoreCase))
				{
					writer.Write(kvp.Key);
					writer.Write(kvp.Value);
				}

				writer.EndCollection();
			}
		}

		private void WriteStrings(ModuleWriter writer, MemberTable<string> strings)
		{
			if (strings.Count == 0)
				writer.Write(0u); // size
			else
			{
				writer.BeginCollection(strings.Count);

				foreach (var str in strings)
				{
					writer.Write(str.Key); // id
					writer.Write(str.Value); // value
				}

				writer.EndCollection();
			}
		}

		private void WriteModuleRefs(ModuleWriter writer, MemberTable<Module> moduleRefs)
		{
			if (moduleRefs.Count == 0)
			{
				writer.Write(0u); // size
			}
			else
			{
				writer.BeginCollection(moduleRefs.Count);

				foreach (var modRef in moduleRefs)
				{
					writer.Write(modRef.Key); // id
					writer.Write(GetStringId(modRef.Value.name)); // name
					writer.Write(modRef.Value.version); // minVersion
				}

				writer.EndCollection();
			}
		}

		private void WriteTypeRefs(ModuleWriter writer, MemberTable<Type> typeRefs)
		{
			if (typeRefs.Count == 0)
			{
				writer.Write(0u); // size
			}
			else
			{
				writer.BeginCollection(typeRefs.Count);

				foreach (var typeRef in typeRefs)
				{
					writer.Write(typeRef.Key); // id
					writer.Write(GetStringId(typeRef.Value.FullName)); // name
					writer.Write(members.ModuleRefs.GetId(typeRef.Value.Module)); // declModule (ID)
				}

				writer.EndCollection();
			}
		}

		private void WriteFunctionRefs(ModuleWriter writer, MemberTable<MethodGroup> functionRefs)
		{
			if (functionRefs.Count == 0)
			{
				writer.Write(0u); // size
			}
			else
			{
				writer.BeginCollection(functionRefs.Count);

				foreach (var funcRef in functionRefs)
				{
					writer.Write(funcRef.Key); // id
					writer.Write(GetStringId(funcRef.Value.FullName)); // name
					writer.Write(members.ModuleRefs.GetId(funcRef.Value.Module)); // declModule (ID)
				}

				writer.EndCollection();
			};
		}

		private void WriteFieldRefs(ModuleWriter writer, MemberTable<Field> fieldRefs)
		{
			if (fieldRefs.Count == 0)
			{
				writer.Write(0u);
			}
			else
			{
				writer.BeginCollection(fieldRefs.Count);

				foreach (var fieldRef in fieldRefs)
				{
					writer.Write(fieldRef.Key); // id
					writer.Write(GetStringId(fieldRef.Value.Name)); // name
					writer.Write(fieldRef.Value.Parent.Id); // declType
				}

				writer.EndCollection();
			}
		}

		private void WriteMethodRefs(ModuleWriter writer, MemberTable<MethodGroup> methodRefs)
		{
			if (methodRefs.Count == 0)
			{
				writer.Write(0u);
			}
			else
			{
				writer.BeginCollection(methodRefs.Count);

				foreach (var methodRef in methodRefs)
				{
					writer.Write(methodRef.Key); // id
					writer.Write(GetStringId(methodRef.Value.Name)); // name
					writer.Write(methodRef.Value.ParentAsClass.Id); // declType
				}

				writer.EndCollection();
			}
		}

		private void WriteTypeDefs(ModuleWriter writer, MemberTable<Type> typeDefs)
		{
			if (typeDefs.Count == 0)
			{
				writer.Write(0u);
			}
			else
			{
				writer.BeginCollection(typeDefs.Count);

				foreach (var type in typeDefs)
				{
					writer.Write(type.Key); // id

					if (type.Value is Enum)
						WriteEnumDef(writer, (Enum)type.Value);
					else
						WriteClassDef(writer, (Class)type.Value);
				}

				writer.EndCollection();
			}
		}

		private void WriteEnumDef(ModuleWriter writer, Enum type)
		{
			var flags = TypeFlags.Primitive |
				(type.Access == AccessLevel.Public ? TypeFlags.Public : TypeFlags.Private);
			writer.WriteFlags(flags); // flags
			writer.Write(GetStringId(type.FullName)); // name

			writer.Write(type.BaseType.Id); // baseType
			writer.Write(0u); // sharedType (always 0 for enums)

			writer.Write(type.members.Count); // memberCount

			// fields
			writer.BeginCollection(type.members.Count);

			foreach (var field in type.GetFieldsSorted())
			{
				writer.Write(field.Id); // id
				writer.WriteFlags(FieldFlags.Public | FieldFlags.HasValue); // flags
				writer.Write(GetStringId(field.Name)); // name
				WriteConstantValue(writer, ConstantValue.CreateEnumValue(field.Value.IntValue, type)); // value
			}

			writer.EndCollection();

			writer.Write(0u); // methods
			writer.Write(0u); // properties
			writer.Write(0u); // operators
			writer.Write(0); // initer (always empty for enums)
		}

		private void WriteClassDef(ModuleWriter writer, Class type)
		{
			var flags = type.Access == AccessLevel.Public ?
				TypeFlags.Public :
				TypeFlags.Private;
			if (type.IsPrimitive)
				flags |= TypeFlags.Primitive;
			else if (type.IsStatic)
				flags |= TypeFlags.Static;
			else if (type.IsAbstract)
				flags |= TypeFlags.Abstract;
			else if (!type.IsInheritable)
				flags |= TypeFlags.Sealed;

			writer.WriteFlags(flags); // flags
			writer.Write(GetStringId(type.FullName)); // name

			writer.Write(type.BaseType == null ? 0u : type.BaseType.Id); // baseType
			writer.Write(type.SharedType == null ? 0u : type.SharedType.Id); // sharedType

			// Collect the different member types
			var fields = new List<ClassMember>();
			var methods = new List<MethodGroup>();
			var properties = new List<Property>();

			foreach (var member in type.GetMembersSorted())
			{
				if (member.Kind == MemberKind.Field)
					fields.Add((Field)member);
				else if (member.Kind == MemberKind.Constant)
					fields.Add((ClassConstant)member);
				else if (member is Property)
					// Note: indexers also claim to be of kind MemberKind.Property;
					// we ignore them in this loop
					properties.Add((Property)member);
				else if (member.Kind == MemberKind.MethodGroup) // MethodGroup
					methods.Add((MethodGroup)member);
			}

			// memberCount
			writer.Write(fields.Count +
				methods.Count +
				properties.Count +
				(type.Indexer != null ? 1 : 0));

			// fields
			if (fields.Count == 0)
				writer.Write(0u);
			else
			{
				writer.BeginCollection(fields.Count);
				foreach (var field in fields)
				{
					if (field.Kind == MemberKind.Field)
					{
						writer.Write(((Field)field).Id);
						writer.WriteFlags(GetFieldFlags((Field)field));
					}
					else
					{
						writer.Write(((ClassConstant)field).Id);
						writer.WriteFlags(GetFieldFlags((ClassConstant)field));
					}

					writer.Write(GetStringId(field.Name));

					if (field.Kind == MemberKind.Constant)
						WriteConstantValue(writer, ((ClassConstant)field).Value);
				}
				writer.EndCollection();
			}

			// methods
			if (methods.Count == 0)
				writer.Write(0u);
			else
			{
				writer.BeginCollection(methods.Count);
				foreach (var method in methods)
					WriteMethodGroup(writer, method);
				writer.EndCollection();
			}

			// properties
			if (properties.Count == 0 && type.Indexer == null)
				writer.Write(0u);
			else
			{
				writer.BeginCollection(properties.Count + (type.Indexer == null ? 0 : 1));

				if (type.Indexer != null)
				{
					writer.Write(GetStringId(type.Indexer.Name)); // name
					writer.Write(type.Indexer.GetterId); // getter
					writer.Write(type.Indexer.SetterId); // setter
				}

				foreach (var prop in properties)
				{
					writer.Write(GetStringId(prop.Name)); // name
					writer.Write(prop.GetterId); // getter
					writer.Write(prop.SetterId); // setter
				}

				writer.EndCollection();
			}

			// operators
			if (type.OverloadedOperatorCount == 0)
				writer.Write(0u); // None shall pass.
			else
			{
				writer.BeginCollection(type.OverloadedOperatorCount);

				for (var i = 0; i < Class.OperatorCount; i++)
				{
					if (type.operators[i] == null)
						continue;
					// The index corresponds exactly to the values in
					// the file format spec's Operator enum
					writer.WriteFlags((Operator)i); // operator
					writer.Write(type.operators[i].Method.Group.Id); // method
				}

				writer.EndCollection();
			}

			// initer
			if (type.Initializer == null)
				writer.Write(0);
			else
				writer.WriteCString(type.Initializer);
		}

		private void WriteMethodGroup(ModuleWriter writer, MethodGroup method)
		{
			writer.Write(method.Id); // Start with the ID

			writer.WriteFlags(GetMethodFlags(method)); // flags
			string name;
			if (method.ParentAsClass == null)
				name = method.FullName; // full name (global function)
			else
				name = method.Name == "this" ? ".call" : method.Name; // name (class method)
			writer.Write(GetStringId(name));

			writer.BeginCollection(method.Count); // overloads!

			foreach (var overload in method)
			{
				var flags = GetOverloadFlags(overload);
				writer.WriteFlags(flags); // flags

				writer.Write((ushort)overload.Signature.ParameterCount); // paramCount
				for (var i = 0; i < overload.Signature.ParameterCount; i++) // paramNames
					writer.Write(GetStringId(overload.Parameters[i].DeclaredName));

				if ((flags & OverloadFlags.ShortHeader) == 0)
					WriteOverloadHeader(writer, overload);

				if (!overload.IsAbstract)
				{
					if ((flags & OverloadFlags.Native) == OverloadFlags.Native)
						writer.WriteCString(((ExternBlockSpace)overload.Body).EntryPoint);
					else
					{
						writer.Write(overload.BodyOffset); // offset
						writer.Write((uint)overload.BodyLength); // length
					}
				}
			}

			writer.EndCollection();
		}

		private void WriteOverloadHeader(ModuleWriter writer, Method overload)
		{
			writer.Write((ushort)overload.Signature.OptionalParameterCount); // optionalParamCount
			writer.Write((ushort)overload.LocalCount); // localCount
			writer.Write((ushort)overload.MaxStack); // maxStack

			// tries
			if (overload.TryBlocks == null)
				writer.Write(0u); // size
			else
			{
				var flatTries = new List<TryBlock>();

				Action<List<TryBlock>, IEnumerable<TryBlock>> flattenTries = null;
				flattenTries = (list, tries) =>
				{
					foreach (var @try in tries)
					{
						if (@try.HasChildTryBlocks)
							flattenTries(list, @try.childTryBlocks);

						list.Add(@try);

						if (@try.Kind == TryBlockKind.TryFinally)
						{
							if (@try.Finally.HasChildTryBlocks)
								flattenTries(list, @try.Finally.childTryBlocks);
						}
						else // try-catch
						{
							foreach (var @catch in @try.Catches)
								if (@catch.HasChildTryBlocks)
									flattenTries(list, @catch.childTryBlocks);
						}
					}
				};
				flattenTries(flatTries, overload.TryBlocks);

				writer.BeginCollection(flatTries.Count);

				foreach (var @try in flatTries)
				{
					writer.Write((byte)@try.Kind); // kind
					writer.Write((uint)@try.StartOffset); // tryStart
					writer.Write((uint)@try.EndOffset); // tryEnd

					if (@try.Kind == TryBlockKind.TryFinally)
					{
						writer.Write((uint)@try.Finally.StartOffset); // finallyStart
						writer.Write((uint)@try.Finally.EndOffset); // finallyEnd
					}
					else // try-catch
					{
						writer.BeginCollection(@try.Catches.Count);

						foreach (var @catch in @try.Catches)
						{
							writer.Write(@catch.TypeId); // caughtType
							writer.Write(@catch.StartOffset); // catchStart
							writer.Write(@catch.EndOffset); // catchEnd
						}

						writer.EndCollection();
					}
				}

				writer.EndCollection();
			}
		}

		private void WriteFunctionDefs(ModuleWriter writer, MemberTable<MethodGroup> functions)
		{
			if (functions.Count == 0)
			{
				writer.Write(0u); // size
			}
			else
			{
				writer.BeginCollection(functions.Count);

				foreach (var func in functions)
					WriteMethodGroup(writer, func.Value);

				writer.EndCollection();
			}
		}

		private void WriteConstantDefs(ModuleWriter writer, MemberTable<GlobalConstant> constants)
		{
			if (constants.Count == 0)
			{
				writer.Write(0u); // size
			}
			else
			{
				writer.BeginCollection(constants.Count);

				foreach (var kvp in constants)
				{
					writer.Write(kvp.Key); // id

					var constant = kvp.Value;
					writer.WriteFlags(constant.Access == AccessLevel.Private ? ConstantFlags.Private : ConstantFlags.Public); // flags

					writer.Write(GetStringId(constant.FullName)); // name
					WriteConstantValue(writer, constant.Value);
				}

				writer.EndCollection();
			}
		}

		private void WriteConstantValue(ModuleWriter writer, ConstantValue value)
		{
			var typeObject = value.GetTypeObject(pool.Compiler);
			if (typeObject == null)
				writer.Write(0u); // null
			else
				writer.Write(typeObject.Id); // type ID

			// And then the value
			switch (value.Type)
			{
				case ConstantValueType.Null:
					writer.Write(0LU); // all zeroes, baby
					break;
				case ConstantValueType.Boolean:
					writer.Write(value.BooleanValue ? 1LU : 0LU);
					break;
				case ConstantValueType.Int:
					writer.Write(value.IntValue);
					break;
				case ConstantValueType.UInt:
					writer.Write(value.UIntValue);
					break;
				case ConstantValueType.Real:
					writer.Write(value.RealValue);
					break;
				case ConstantValueType.String:
					writer.Write((ulong)GetStringId(value.StringValue));
					break;
				case ConstantValueType.Enum:
					writer.Write(value.EnumValue.Value);
					break;
			}
		}

		private FieldFlags GetFieldFlags(Field field)
		{
			return (field.Access == AccessLevel.Public ? FieldFlags.Public :
				field.Access == AccessLevel.Private ? FieldFlags.Private :
				FieldFlags.Protected)
				|
				(field.IsStatic ? 0 : FieldFlags.Instance);
		}

		private FieldFlags GetFieldFlags(ClassConstant constant)
		{
			return (constant.Access == AccessLevel.Public ? FieldFlags.Public :
				constant.Access == AccessLevel.Private ? FieldFlags.Private :
				FieldFlags.Protected)
				|
				FieldFlags.HasValue;
		}

		private MethodFlags GetMethodFlags(MethodGroup method)
		{
			var flags = method.Access == AccessLevel.Public ? MethodFlags.Public :
				method.Access == AccessLevel.Private ? MethodFlags.Private :
				MethodFlags.Protected;
			if (!method.IsStatic)
				flags |= MethodFlags.Instance;
			if (method.Name == ".new" || method.Name == ".init")
				flags |= MethodFlags.Ctor;
			if (method.Any(o => o.IsImplDetail))
				flags |= MethodFlags.Impl;
			return flags;
		}

		private OverloadFlags GetOverloadFlags(Method overload)
		{
			var flags = overload.Body is ExternBlockSpace ? OverloadFlags.Native : 0;

			if (overload.Signature.Splat == Nodes.Splat.Beginning)
				flags |= OverloadFlags.VarStart;
			else if (overload.Signature.Splat == Nodes.Splat.End)
				flags |= OverloadFlags.VarEnd;
			if (overload.IsOverridable)
				flags |= OverloadFlags.Virtual;
			else if (overload.IsAbstract)
				flags |= OverloadFlags.Abstract;

			if (overload.LocalCount == 0 && overload.MaxStack <= 8 &&
				overload.TryBlocks == null && overload.Signature.OptionalParameterCount == 0)
				flags |= OverloadFlags.ShortHeader;

			return flags;
		}
	}
}