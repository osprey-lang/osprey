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

			var reader = new ModuleReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read), Encoding.Unicode);
			var fileFormatVersion = reader.SkipHeader();
			if (fileFormatVersion < MinFileFormatVersion ||
				fileFormatVersion > MaxFileFormatVersion)
				throw new ModuleLoadException(fileName,
					string.Format("Unsupported module file version number (found {0:X4}, should be between {1:X4} and {2:X4}).",
						fileFormatVersion, MinFileFormatVersion, MaxFileFormatVersion));

			// First the module name and version
			var modName = reader.ReadOvumString();
			var modVersion = reader.ReadVersion();
			if (requiredVersion != null && modVersion != requiredVersion)
				throw new ModuleLoadException(fileName,
					string.Format("Wrong version number for module (expected {0}, found {1}).",
						requiredVersion.ToStringInvariant(4), modVersion.ToStringInvariant(4)));

			// Then the metadata table
			var metaSize = reader.ReadUInt32(); // The size of the (rest of the) metadata table, let's skip it
			reader.Seek(metaSize, SeekOrigin.Current);

			// Then a native library, if any (not used)
			var nativeLibStrlen = reader.ReadInt32();
			reader.Seek(/*sizeof(UTF-16 codon)*/ 2 * nativeLibStrlen, SeekOrigin.Current);

			// We have enough information to create a Module object, and populate it with some basic data!
			var output = new Module(pool, modName, modVersion, fileFormatVersion, true);
			pool.AddModule(output.name, output, fromVersionedFile); // For detecting circular dependencies

			// Skip typeCount, functionCount, constantCount, fieldCount, methodCount and methodStart
			reader.Seek(6 * sizeof(int), SeekOrigin.Current);

			// First is the string table, which we must read, because all member names use this,
			// and it may be used in constants, too.
			ReadStringTable(reader, output);

			// Now come a bunch of ref tables. In order:
			//    1. moduleRefs
			//    2. typeRefs
			//    3. functionRefs
			//    4. fieldRefs
			//    5. methodRefs
			// Of these tables, we only need to care about moduleRefs and typeRefs,
			// for the purposes of resolving type IDs in base types and constants.
			// ALL other member IDs appear ONLY in method bodies, which we don't load.

			ReadModuleRefs(reader, output); // moduleRefs
			ReadTypeRefs(reader, output); // typeRefs

			reader.SkipCollection(); // functionRefs
			reader.SkipCollection(); // fieldRefs
			reader.SkipCollection(); // methodRefs

			// And now we have types, functions and constants
			ReadTypeDefs(reader, output);
			if (output.unresolvedConstants != null)
				foreach (var constant in output.unresolvedConstants)
				{
					Type typeObject;
					var constType = GetConstantType(reader, output, constant.TypeId, out typeObject);
					var constValue = ConstantValueFromRaw(constType, output, typeObject, constant.Value);
					constant.Member.UpdateValue(constValue);
				}

			// Functions
			ReadFunctionDefs(reader, output);

			// And finally, constants
			ReadGlobalConstantDefs(reader, output);

			// Technically, the main method and all the method bodies follow here,
			// but we're not interested in either, so we just ignore everything else.
			// Hurrah, we're done!

			// Though first we lock all the tables we used.
			output.members.ModuleRefs.Lock();
			output.members.TypeRefs.Lock();
			output.members.TypeDefs.Lock();

			output.fullyLoaded = true;
			return output;
		}

		private static void ReadStringTable(ModuleReader reader, Module target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32(); // number of table entries

				var strings = target.members.Strings;

				for (var i = 0; i < length; i++)
				{
					uint id = reader.ReadUInt32();
					if (id != (StringMask | unchecked((uint)i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid String token ID (must be consecutive).");

					var value = reader.ReadOvumString();
					strings.Add(value);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "String table reports an inaccurate size.");
			}
		}

		private static void ReadModuleRefs(ModuleReader reader, Module target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32(); // number of table entries

				var modRefs = target.members.ModuleRefs;

				for (var i = 0; i < length; i++)
				{
					uint id = reader.ReadUInt32();
					if (id != (ModuleRefMask | unchecked((uint)i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid ModuleRef token ID (must be consecutive).");

					var refName = target.members.Strings[reader.ReadUInt32()];
					var refVersion = reader.ReadVersion();

					var mod = target.Pool.GetOrLoad(refName, refVersion);
					if (!mod.fullyLoaded)
						throw new ModuleLoadException(reader.FileName,
							string.Format("Circular dependency between modules '{0}' and '{1}'.",
								mod.name, target.name));
					// We no longer have to check the version; Module.Open gets the required version
					// and throws if we can't find the right one.

					modRefs.Add(mod);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "ModuleRef table reports an inaccurate size.");
			}
		}

		private static void ReadTypeRefs(ModuleReader reader, Module target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32(); // number of table entries

				var modRefs = target.members.ModuleRefs;
				var typeRefs = target.members.TypeRefs;

				for (var i = 0; i < length; i++)
				{
					uint id = reader.ReadUInt32();
					if (id != (TypeRefMask | unchecked((uint)i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid TypeRef token ID (must be consecutive).");

					var typeName = target.members.Strings[reader.ReadUInt32()];
					var declModule = reader.ReadUInt32();
					var type = target.GetModule(declModule).GetType(typeName);

					typeRefs.Add(type);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "TypeRef table reports an inaccurate size.");
			}
		}

		private static void ReadTypeDefs(ModuleReader reader, Module target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32(); // number of table entries

				var typeDefs = target.members.TypeDefs;

				uint fieldCounter = 0, methodCounter = 0;

				for (var i = 0; i < length; i++)
				{
					uint id = reader.ReadUInt32();
					if (id != (TypeDefMask | unchecked((uint)i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid TypeDef token ID (must be consecutive).");

					var type = ReadSingleTypeDef(reader, target, id, ref fieldCounter, ref methodCounter);
					typeDefs.Add(type);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "TypeDef table reports an inaccurate size.");
			}
		}

		private static Type ReadSingleTypeDef(ModuleReader reader, Module target, uint id,
			ref uint fieldCounter, ref uint methodCounter)
		{
			var flags = reader.ReadTypeFlags();

			var fullName = target.members.Strings[reader.ReadUInt32()];

			uint baseTypeId = reader.ReadUInt32();
			uint sharedTypeId = reader.ReadUInt32();

			if (baseTypeId == 0 && fullName != StandardNames.TypeRootName)
				throw new ModuleLoadException(reader.FileName,
					string.Format("Only the root of the type hierarchy ('{0}') may have no base type.", StandardNames.TypeRootName));
			if (baseTypeId != 0 && fullName == StandardNames.TypeRootName)
				throw new ModuleLoadException(reader.FileName,
					string.Format("The root of the type hierarchy ('{0}') may not have a base type.", StandardNames.TypeRootName));

			Type baseType = null;
			if ((MaskMask & baseTypeId) == TypeDefMask)
				baseType = target.members.TypeDefs[baseTypeId];
			else if (baseTypeId != 0)
				baseType = target.members.TypeRefs[baseTypeId];

			var sharedType = sharedTypeId == 0 ? null : target.members.TypeDefs[sharedTypeId];

			string typeName;
			if (target.explicitlyImported)
			{
				var nameParts = fullName.Split('.');
				typeName = nameParts[nameParts.Length - 1];
			}
			else
			{
				typeName = fullName;
			}

			reader.ReadInt32(); // Skip memberCount

			Type output;
			var baseTypeName = baseType == null ? null : baseType.FullName;
			if (baseTypeName == StandardNames.EnumName && fullName != StandardNames.EnumSetName ||
				baseTypeName == StandardNames.EnumSetName)
				// Ignore the shared type; enums don't need to share types with anything, man.
				output = ReadEnumDef(reader, target, id, flags, typeName, baseType, baseTypeName == StandardNames.EnumSetName,
					ref fieldCounter, ref methodCounter);
			else
				output = ReadClassDef(reader, target, flags, typeName, baseType, sharedType,
					ref fieldCounter, ref methodCounter);
			output.Module = target;
			output.BaseType = baseType;

			if (target.explicitlyImported && output.Access == AccessLevel.Public)
			{
				var namespacePath = GetPathFromName(fullName, out typeName);
				var targetNs = target.pool.Namespace.GetNamespace(namespacePath, true);

				output.Parent = targetNs;
				targetNs.DeclareType(output, true);
			}

			target.membersByFullName[fullName] = output;

			return output;
		}

		private static Class ReadClassDef(ModuleReader reader, Module target,
			TypeFlags flags, string typeName, Type baseType, Type sharedType,
			ref uint fieldCounter, ref uint methodCounter)
		{
			var output = new Class(typeName,
				(flags & TypeFlags.Private) == TypeFlags.Private ? AccessLevel.Private : AccessLevel.Public,
				parent: target.explicitlyImported ? target.pool.Namespace : null);

			output.IsPrimitive = (flags & TypeFlags.Primitive) == TypeFlags.Primitive;
			if ((flags & TypeFlags.Static) == TypeFlags.Static) // static = sealed | abstract
				output.IsStatic = true;
			else
			{
				output.IsInheritable = (flags & TypeFlags.Sealed) == 0;
				output.IsAbstract = (flags & TypeFlags.Abstract) == TypeFlags.Abstract;
			}

			// Fields first
			ReadFieldDefs(reader, target, output, ref fieldCounter);
			// Then methods
			ReadMethodDefs(reader, target, output, ref methodCounter);
			// Then properties
			ReadPropertyDefs(reader, target, output);
			// Then operators
			ReadOperators(reader, target, output);
			// Then type initializer (skip)
			var initerLength = reader.ReadInt32();
			reader.Seek(initerLength, SeekOrigin.Current);

			return output;
		}

		private static void ReadFieldDefs(ModuleReader reader, Module module, Class target, ref uint fieldCounter)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32(); // number of fields in this type

				for (var i = 0; i < length; i++)
				{
					var id = reader.ReadUInt32();
					if (id != (FieldDefMask | (fieldCounter + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid FieldDef token ID (must be consecutive).");
					fieldCounter++;

					var flags = reader.ReadFieldFlags();
					var name = module.members.Strings[reader.ReadUInt32()];

					if ((flags & FieldFlags.HasValue) == FieldFlags.HasValue)
					{
						var field = ReadClassConstant(reader, module, target, flags, name);
						target.DeclareConstant(field);
					}
					else
					{
						var field = new Field(name, GetAccessibility(reader, flags), target);
						target.DeclareField(field);
					}
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "FieldDef table reported an inaccurate size.");
			}
		}

		private static ImportedClassConstant ReadClassConstant(ModuleReader reader, Module module, Class target, FieldFlags flags, string name)
		{
			var typeId = reader.ReadUInt32();
			var value = reader.ReadInt64();

			if (typeId == 0) // null
				// Ignore the value.
				return new ImportedClassConstant(name, GetAccessibility(reader, flags), target, ConstantValue.Null);

			Type type;
			bool success;
			var constantType = GetConstantType(reader, module, typeId, out type, out success);

			ConstantValue constValue;
			if (success)
				constValue = ConstantValueFromRaw(constantType, module, type, value);
			else
				constValue = ConstantValue.Null;

			var output = new ImportedClassConstant(name, GetAccessibility(reader, flags), target, constValue);
			if (!success)
				module.AddUnresolvedConstant(output, typeId, value);

			return output;
		}

		private static void ReadMethodDefs(ModuleReader reader, Module module, Class target, ref uint methodCounter)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				var methodDefs = module.members.MethodDefs;

				for (var i = 0; i < length; i++)
				{
					var id = reader.ReadUInt32();
					if (id != (MethodDefMask | (methodCounter + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid MethodDef token ID (must be consecutive).");
					methodCounter++;

					var method = ReadSingleMethodDef(reader, module, target);

					target.ImportMethodGroup(method);
					if (method.Name == Constructor.InstanceCtorName)
						target.Constructors = method;
					methodDefs.Add(method);
					method.Module = module;
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "MethodDef table reported an inaccurate size.");
			}
		}

		private static MethodGroup ReadSingleMethodDef(ModuleReader reader, Module module, NamedMember parent)
		{
			var methodFlags = reader.ReadMethodFlags();
			var name = module.members.Strings[reader.ReadUInt32()];

			// overloads!
			var overloadsSize = reader.ReadUInt32();
			if (overloadsSize == 0)
				throw new ModuleLoadException(reader.FileName, "Encountered a MethodDef with no overloads.");

			var access = GetAccessibility(reader, methodFlags);
			if (parent is Namespace)
			{
				// Global function in explicitly imported module;
				// the parent is the global declaration space.
				var namespacePath = GetPathFromName(name, out name);

				if (namespacePath.Length > 0)
					parent = ((Namespace)parent).GetNamespace(namespacePath, true);
			}
			var group = new MethodGroup(name, parent, access);

			var posBefore = reader.BaseStream.Position;
			var length = reader.ReadInt32();

			for (var i = 0; i < length; i++)
			{
				var flags = reader.ReadOverloadFlags();

				int paramCount = reader.ReadUInt16();
				var parameters = new Parameter[paramCount];
				for (var p = 0; p < paramCount; p++)
				{
					var paramName = module.members.Strings[reader.ReadUInt32()];
					var paramFlags = reader.ReadParamFlags();
					parameters[p] = new Parameter(paramName,
						(paramFlags & ParamFlags.ByRef) == ParamFlags.ByRef);
				}

				ushort optionalParamCount;
				if ((flags & OverloadFlags.ShortHeader) == OverloadFlags.ShortHeader)
					optionalParamCount = 0;
				else
				{
					optionalParamCount = reader.ReadUInt16();
					// Skip localCount + maxStack (both ushorts)
					reader.Seek(4, SeekOrigin.Current);
					reader.SkipCollection(); // Try blocks (not needed)
				}

				if ((flags & OverloadFlags.Native) == OverloadFlags.Native)
				{
					// Skip entry point name
					int entryPointLength = reader.ReadInt32();
					reader.Seek(entryPointLength, SeekOrigin.Current);
				}
				else if ((flags & OverloadFlags.Abstract) == 0)
					// Skip offset + length (both uints)
					reader.Seek(8,  SeekOrigin.Current);

				var signature = new Signature(paramCount, optionalParamCount,
					(flags & OverloadFlags.VarStart) == OverloadFlags.VarStart ? Splat.Beginning :
					(flags & OverloadFlags.VarEnd) == OverloadFlags.VarEnd ? Splat.End :
					Splat.None);

				var overload = parent is Class ?
					new ClassMemberMethod(null, name, null, access, null, signature) :
					new Method(null, name, access, null, signature);

				overload.Parameters = parameters;
				overload.IsAbstract = (flags & OverloadFlags.Abstract) == OverloadFlags.Abstract;
				overload.IsStatic = (methodFlags & MethodFlags.Instance) == 0;
				overload.IsImplDetail = (methodFlags & MethodFlags.Impl) == MethodFlags.Impl;
				overload.IsOverridable = (flags & OverloadFlags.Virtual) == OverloadFlags.Virtual;
				// The file format does not explicitly state whether the overload overrides anything,
				// so we override opportunistically, except we do that in ImportMethodGroup.

				if ((methodFlags & MethodFlags.Ctor) == MethodFlags.Ctor)
					overload.Flags |= MemberFlags.Constructor;
				group.AddOverload(overload);
			}

			if (reader.BaseStream.Position != posBefore + overloadsSize)
				throw new ModuleLoadException(reader.FileName, "MethodDef's overload list reported an inaccurate size.");

			if (parent is Namespace && access != AccessLevel.Private)
				((Namespace)parent).ImportMethodGroup(group);

			return group;
		}

		private static void ReadPropertyDefs(ModuleReader reader, Module module, Class target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				// Note: this is a List<PropertyDef>, not Table<PropertyDef>.
				for (var i = 0; i < length; i++)
				{
					var name = module.members.Strings[reader.ReadUInt32()];
					var getterId = reader.ReadUInt32();
					var setterId = reader.ReadUInt32();

					if (getterId == 0 && setterId == 0)
						throw new ModuleLoadException(reader.FileName, "PropertyDef with no accessors.");

					MethodGroup getterGroup = null, setterGroup = null;
					if (getterId != 0)
					{
						getterGroup = module.members.MethodDefs[getterId];
						if (getterGroup.ParentAsClass != target)
							throw new ModuleLoadException(reader.FileName,
								"PropertyDef accessors must refer to a method declared in the containing class.");
					}
					if (setterId != 0)
					{
						setterGroup = module.members.MethodDefs[setterId];
						if (setterGroup.ParentAsClass != target)
							throw new ModuleLoadException(reader.FileName,
								"PropertyDef accessors must refer to a method declared in the containing class.");
					}

					if (name == Indexer.MemberName) // indexer
					{
						var indexer = CreateIndexer(target, getterGroup, setterGroup);
						target.ImportIndexer(indexer);
					}
					else // other property
					{
						Method getter = null, setter = null;
						if (getterGroup != null)
						{
							getter = getterGroup.FindOverload(Signature.Empty);
							if (getter == null)
								throw new ModuleLoadException(reader.FileName, "Could not find an overload for property getter.");
						}
						if (setterGroup != null)
						{
							setter = setterGroup.FindOverload(Signature.OneRequired);
							if (setter == null)
								throw new ModuleLoadException(reader.FileName, "Could not find an overload for property setter.");
						}

						var prop = new Property(name, target);
						if (getter != null)
							prop.Getter = new PropertyAccessor((ClassMemberMethod)getter, isSetter: false, isIndexer: false);
						if (setter != null)
							prop.Setter = new PropertyAccessor((ClassMemberMethod)setter, isSetter: true, isIndexer: false);
						target.ImportProperty(prop);
					}
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "PropertyDef table reported an inaccurate size.");
			}
		}

		private static IndexerMember CreateIndexer(Class owner, MethodGroup getterGroup, MethodGroup setterGroup)
		{
			var indexerMember = new IndexerMember(owner);

			var indexers = new Dictionary<int, Indexer>(Math.Max(
				getterGroup == null ? 0 : getterGroup.Count,
				setterGroup == null ? 0 : setterGroup.Count));

			if (getterGroup != null)
				foreach (var ovl in getterGroup)
				{
					var cmm = ovl as ClassMemberMethod;
					if (cmm != null)
					{
						var argCount = cmm.Parameters.Length;
						Indexer indexer;
						if (!indexers.TryGetValue(argCount, out indexer))
							indexers.Add(argCount, indexer = new Indexer(owner, argCount));

						indexer.Getter = new IndexerAccessor(argCount, cmm, isSetter: false);
					}
				}

			if (setterGroup != null)
				foreach (var ovl in setterGroup)
				{
					var cmm = ovl as ClassMemberMethod;
					if (cmm != null)
					{
						var argCount = cmm.Parameters.Length - 1; // -1 for the value parameter
						Indexer indexer;
						if (!indexers.TryGetValue(argCount, out indexer))
							indexers.Add(argCount, indexer = new Indexer(owner, argCount));

						indexer.Setter = new IndexerAccessor(argCount, cmm, isSetter: true);
					}
				}

			foreach (var indexer in indexers.Values)
				indexerMember.AddIndexer(indexer);

			return indexerMember;
		}

		private static void ReadOperators(ModuleReader reader, Module module, Class target)
		{
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				Func<Operator, int> getArity = op =>
				{
					if (op == Operator.Plus || op == Operator.Negate ||
						op == Operator.Not)
						return 1;
					return 2;
				};

				for (var i = 0; i < length; i++)
				{
					var op = reader.ReadOperator();
					var methodId = reader.ReadUInt32();

					if ((byte)op > (byte)Operator.Compare)
						throw new ModuleLoadException(reader.FileName, "Invalid operator in OperatorDef.");
					if (target.operators[(int)op] != null)
						throw new ModuleLoadException(reader.FileName, "Duplicate operator definition.");

					var methodGroup = module.members.MethodDefs[methodId];
					var method = methodGroup.FindOverload(new Signature(getArity(op), 0, Splat.None));
					if (method == null)
						throw new ModuleLoadException(reader.FileName, "Could not find an overload for OperatorDef.");

					var overload = new OperatorOverload((int)op, target, (ClassMemberMethod)method);
					target.ImportOperatorOverload(overload);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "OperatorDef list reported an inaccurate size.");
			}
		}

		private static Type ReadEnumDef(ModuleReader reader, Module target, uint typeId,
			TypeFlags typeFlags, string typeName, Type baseType, bool isSet,
			ref uint fieldCounter, ref uint methodCounter)
		{
			var output = new Enum(typeName, GetAccessibility(reader, typeFlags), isSet,
				parent: target.explicitlyImported ? target.pool.Namespace : null);

			// Fields
			var size = reader.ReadUInt32();
			if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				for (var i = 0; i < length; i++)
				{
					var id = reader.ReadUInt32();
					if (id != (FieldDefMask | (fieldCounter + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid FieldDef token ID (must be consecutive).");
					fieldCounter++;

					var flags = reader.ReadFieldFlags();
					var name = target.members.Strings[reader.ReadUInt32()];

					var access = GetAccessibility(reader, flags);
					if (access != AccessLevel.Public)
					{
						// Ignore non-public fields
						if ((flags & FieldFlags.HasValue) == FieldFlags.HasValue)
							// Skip constant type (uint) + constant value (ulong)
							reader.Seek(4 + 8, SeekOrigin.Current);
					}
					else if ((flags & FieldFlags.HasValue) == FieldFlags.HasValue)
					{
						var constId = reader.ReadUInt32();
						if (constId != typeId)
							// ignore the field
							reader.ReadUInt64(); // Skip value (ulong)
						else
						{
							var value = reader.ReadInt64();
							output.DeclareField(new EnumField(name, value, output));
						}
					}
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "FieldDef table reported an inaccurate size.");
			}
			// Methods (ignore)
			size = reader.ReadUInt32();
			if (size != 0)
			{
				var length = reader.ReadInt32();
				methodCounter += unchecked((uint)length);
				target.members.MethodDefs.AddEmpty(length);
				reader.Seek(size - 4, SeekOrigin.Current);
			}
			// Properties (ignore)
			reader.SkipCollection();
			// Operators (ignore)
			reader.SkipCollection();

			// Initer (skip)
			var initerLength = reader.ReadInt32();
			reader.Seek(initerLength, SeekOrigin.Current);

			return output;
		}

		private static void ReadFunctionDefs(ModuleReader reader, Module target)
		{
			// Global functions are only ever referenced in method bodies,
			// so if the target module is not explicitly imported, we don't
			// need to read these. hurrah!
			var size = reader.ReadUInt32();
			if (!target.explicitlyImported)
			{
				reader.Seek(size, SeekOrigin.Current);
			}
			else if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				for (var i = 0; i < length; i++)
				{
					var id = reader.ReadUInt32();
					if (id != (GlobalFuncDefMask | (i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid FunctionDef token ID (must be consecutive).");

					var method = ReadSingleMethodDef(reader, target, target.pool.Namespace);
					//target.members.GlobalFuncDefs.Add(method);
					method.Module = target;
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "FunctionDef table reported an inaccurate size.");
			}
		}

		private static void ReadGlobalConstantDefs(ModuleReader reader, Module target)
		{
			// Global constants are always inlined. If the target module is not explicitly imported,
			// then we don't need to care about the global constants.
			var size = reader.ReadUInt32();
			if (!target.explicitlyImported)
			{
				reader.Seek(size, SeekOrigin.Current);
			}
			else if (size != 0)
			{
				var posBefore = reader.BaseStream.Position;
				var length = reader.ReadInt32();

				for (var i = 0; i < length; i++)
				{
					var id = reader.ReadInt32();
					if (id != (GlobalConstMask | (i + 1)))
						throw new ModuleLoadException(reader.FileName, "Invalid ConstantDef token ID (must be consecutive).");

					var flags = reader.ReadConstantFlags();
					var name = target.members.Strings[reader.ReadUInt32()];

					var typeId = reader.ReadUInt32();
					var value = reader.ReadInt64();
					Type typeObject;
					var constType = GetConstantType(reader, target, typeId, out typeObject);
					var constValue = ConstantValueFromRaw(constType, target, typeObject, value);

					var namespacePath = GetPathFromName(name, out name);
					var parentNs = target.pool.Namespace.GetNamespace(namespacePath, true);
					
					var globalConstant = new GlobalConstant(name, constValue, GetAccessibility(reader, flags));
					
					parentNs.DeclareConstant(globalConstant);
					//target.members.GlobalConstDefs.Add(globalConstant);
				}

				if (reader.BaseStream.Position != posBefore + size)
					throw new ModuleLoadException(reader.FileName, "ConstantDef table reported an inaccurate size.");
			}
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
					throw new ModuleLoadException(reader.FileName, "Could not resolve TypeRef token ID.");
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
				throw new ModuleLoadException(reader.FileName, "Invalid ID kind: must be a TypeDef or TypeRef.");

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
			var nameParts = fullName.Split('.');
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