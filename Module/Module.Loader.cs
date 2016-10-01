using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.ModuleFile;
using Osprey.Nodes;
using Enum = Osprey.Members.Enum;
using Type = Osprey.Members.Type;
using Parameter = Osprey.Nodes.Parameter;
using Raw = Osprey.ModuleFile.Raw;

namespace Osprey
{
	public sealed partial class Module
	{
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

			Module result;

			using (var reader = new ModuleReader(fileName))
			{
				var header = reader.Read<Raw.ModuleHeaderStruct>(0);

				if (header.Magic != Module.MagicNumber)
					throw new ModuleLoadException(fileName, "Invalid magic number in file");
				if (header.FormatVersion < MinFileFormatVersion || header.FormatVersion > MaxFileFormatVersion)
					throw new ModuleLoadException(fileName, "Unsupported file format version: " + header.FormatVersion);
				ReadAnnotations(reader, header.Annotations);

				reader.FieldsBase = header.Fields.Address;
				reader.MethodsBase = header.Methods.Address;

				var name = reader.ReadString(header.Name);
				var version = new Version(
					unchecked((int)header.Version.Major),
					unchecked((int)header.Version.Minor),
					unchecked((int)header.Version.Patch),
					0
				);

				if (requiredVersion != null && version != requiredVersion)
					throw new ModuleLoadException(
						reader.FileName,
						string.Format("Wrong module version (expected {0}, got {1})", requiredVersion, version)
					);

				result = new Module(pool, name, version, header.FormatVersion, true, fromVersionedFile);
				// Add the module to the pool, so we can detect circular dependencies
				pool.AddModule(name, result, fromVersionedFile);

				// First we must read the string table, because every member name refers to this
				ReadStringTable(reader, result, header.Strings);

				// Resolve the module's references too
				ReadReferences(reader, result, header.References);

				// And now we can read type, function and global constant definitions
				ReadDefinitions(reader, result, ref header);

				result.fullyLoaded = true;
			}

			return result;
		}

		private static void ReadStringTable(ModuleReader reader, Module module, Raw.Rva<Raw.StringTableHeaderStruct> rva)
		{
			var header = reader.Deref(rva);

			var address = rva.Address + Raw.StringTableHeaderStruct.StringsOffset;

			for (var i = 0; i < header.Length; i++, address += sizeof(uint))
			{
				var token = MakeToken(StringMask, i);

				var stringAddress = reader.ReadRva<Raw.StringStruct>(address);
				var stringValue = reader.ReadString(stringAddress);

				var actualToken = module.members.Strings.Add(stringValue);
				if (token != actualToken)
					ErrorWrongTokenValue(reader.FileName, actualToken, token);
			}
		}

		private static void ReadReferences(ModuleReader reader, Module module, Raw.Rva<Raw.RefTableHeaderStruct> rva)
		{
			var header = reader.Deref(rva);

			// We only need to read module and type references. Strictly speaking we
			// only care about type references, but we need to know their declaring
			// modules in order to load them. The only reason we need to load type
			// references for imported modules is to resolve base types as well as
			// types of constant values (both class and global constants).
			// All other kinds of references are only used inside method bodies, which
			// we don't care about.

			ReadModuleRefs(reader, module, header.ModuleRefCount, header.ModuleRefs);
			ReadTypeRefs(reader, module, header.TypeRefCount, header.TypeRefs);
		}

		private static void ReadModuleRefs(ModuleReader reader, Module module, int count, Raw.RvaToArray<Raw.ModuleRefStruct> rva)
		{
			if (count == 0)
				return;

			var moduleRefs = module.members.ModuleRefs;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.ModuleRefStruct.Size)
			{
				var token = MakeToken(ModuleRefMask, i);

				var moduleRef = reader.Read<Raw.ModuleRefStruct>(address);

				// This compiler ignores the VersionConstraint and treats everything
				// like an exact version.
				var name = module.members.Strings[moduleRef.Name.Value];
				var version = new Version(
					unchecked((int)moduleRef.Version.Major),
					unchecked((int)moduleRef.Version.Minor),
					unchecked((int)moduleRef.Version.Patch),
					0
				);

				var importedModule = module.Pool.GetOrLoad(name, version);
				if (!importedModule.fullyLoaded)
					throw new ModuleLoadException(
						reader.FileName,
						string.Format("Circular dependency between '{0}' and '{1}'", name, module.name)
					);

				var actualToken = moduleRefs.Add(importedModule);
				if (actualToken != token)
					ErrorWrongTokenValue(reader.FileName, actualToken, token);
			}
		}

		private static void ReadTypeRefs(ModuleReader reader, Module module, int count, Raw.RvaToArray<Raw.TypeRefStruct> rva)
		{
			if (count == 0)
				return;

			var typeRefs = module.members.TypeRefs;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.TypeRefStruct.Size)
			{
				var token = MakeToken(TypeRefMask, i);

				var typeRef = reader.Read<Raw.TypeRefStruct>(address);

				var declModule = module.GetModule(typeRef.DeclModule.Value);
				var name = module.members.Strings[typeRef.Name.Value];
				var type = declModule.GetType(name);

				typeRefs.Add(type);
			}
		}

		private static void ReadDefinitions(ModuleReader reader, Module module, ref Raw.ModuleHeaderStruct moduleHeader)
		{
			ReadTypeDefs(reader, module, moduleHeader.TypeCount, moduleHeader.Types);

			// At this point we have enough information to resolve any class constants
			// that refer to types we hadn't encountered.
			ResolveConstants(reader, module);

			ReadFunctionDefs(reader, module, moduleHeader.FunctionCount, moduleHeader.Functions);

			ReadConstantDefs(reader, module, moduleHeader.ConstantCount, moduleHeader.Constants);
		}

		private static void ReadTypeDefs(
			ModuleReader reader,
			Module module,
			int count,
			Raw.RvaToArray<Raw.TypeDefStruct> rva
		)
		{
			// General note: When reading typedefs, we don't need to concern ourselves
			// with putting fields in module.members.FieldDefs, because they are only
			// ever referred to inside method bodies, which we don't read.
			// However, we DO have to populate module.members.MethodDefs, since methods
			// are referenced by properties and operator overloads, which we do read.

			if (count == 0)
				return;

			var typeDefs = module.members.TypeDefs;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.TypeDefStruct.Size)
			{
				var token = MakeToken(TypeDefMask, i);

				string fullName;
				var type = ReadSingleTypeDef(reader, module, address, token, out fullName);

				// If the type is public and belongs to an explicitly imported module,
				// we must make it available to the developer in its parent namespace.
				// Otherwise it would be impossible to refer to it from code!
				if (module.explicitlyImported && type.Access == AccessLevel.Public)
				{
					string _;
					var namespacePath = GetPathFromName(fullName, out _);
					var targetNs = module.pool.Namespace.GetNamespace(namespacePath, true);

					type.Parent = targetNs;
					targetNs.DeclareType(type, true);
				}

				// Modules that refer to this module also need to be able to reference
				// the type, by full name.
				if (type.Access == AccessLevel.Public)
					module.membersByFullName[type.FullName] = type;

				// And finally lets give it a TypeDef token.
				var actualToken = typeDefs.Add(type);
				if (actualToken != token)
					ErrorWrongTokenValue(reader.FileName, actualToken, token);
			}
		}

		private static Type ReadSingleTypeDef(
			ModuleReader reader,
			Module module,
			uint address,
			uint typeToken,
			out string fullName
		)
		{
			var typeDef = reader.Read<Raw.TypeDefStruct>(address);
			ReadAnnotations(reader, typeDef.Annotations);

			fullName = module.members.Strings[typeDef.Name.Value];

			Type baseType = null;
			if (typeDef.BaseType.Value != 0)
			{
				if (typeDef.BaseType.MemberKind == Raw.MemberKind.TypeDef)
					baseType = module.members.TypeDefs[typeDef.BaseType.Value];
				else
					baseType = module.members.TypeRefs[typeDef.BaseType.Value];
			}

			var baseTypeName = baseType != null ? baseType.FullName : null;
			Type result;
			if (baseTypeName == StandardNames.EnumName || baseTypeName == StandardNames.EnumSetName)
			{
				result = ReadEnumDef(
					reader,
					module,
					ref typeDef,
					typeToken,
					fullName,
					baseType,
					baseTypeName == StandardNames.EnumSetName
				);
			}
			else
			{
				result = ReadClassDef(
					reader,
					module,
					ref typeDef,
					fullName,
					baseType
				);
			}

			result.Module = module;

			return result;
		}

		private static Type ReadClassDef(
			ModuleReader reader,
			Module module,
			ref Raw.TypeDefStruct typeDef,
			string fullName,
			Type baseType
		)
		{
			var result = new Class(
				GetLastNameComponent(fullName),
				GetAccessibility(reader, typeDef.Flags),
				parent: module.explicitlyImported ? module.pool.Namespace : null
			);
			result.BaseType = baseType;

			result.IsPrimitive = (typeDef.Flags & Raw.TypeFlags.Primitive) == Raw.TypeFlags.Primitive;
			if ((typeDef.Flags & Raw.TypeFlags.Static) == Raw.TypeFlags.Static)
			{
				result.IsStatic = true;
			}
			else
			{
				result.IsInheritable = (typeDef.Flags & Raw.TypeFlags.Sealed) == 0;
				result.IsAbstract = (typeDef.Flags & Raw.TypeFlags.Abstract) == Raw.TypeFlags.Abstract;
			}

			ReadClassFields(reader, module, result, typeDef.FieldCount, typeDef.FirstField);

			ReadClassMethods(reader, module, result, typeDef.MethodCount, typeDef.FirstMethod);

			ReadClassProperties(reader, module, result, typeDef.PropertyCount, typeDef.Properties);

			ReadClassOperators(reader, module, result, typeDef.OperatorCount, typeDef.Operators);

			return result;
		}

		private static void ReadClassFields(
			ModuleReader reader,
			Module module,
			Class type,
			int count,
			MetadataToken firstField
		)
		{
			if (count == 0)
				return;

			var address = reader.FieldsBase + Raw.FieldDefStruct.Size * firstField.Index;
			for (var i = 0; i < count; i++, address += Raw.FieldDefStruct.Size)
			{
				var fieldDef = reader.Read<Raw.FieldDefStruct>(address);
				ReadAnnotations(reader, fieldDef.Annotations);

				if ((fieldDef.Flags & Raw.FieldFlags.HasValue) == Raw.FieldFlags.HasValue)
				{
					var field = ReadClassConstant(reader, module, type, ref fieldDef);
					type.DeclareConstant(field);
				}
				else
				{
					var field = ReadClassField(reader, module, type, ref fieldDef);
					type.DeclareField(field);
				}
			}
		}

		private static Field ReadClassField(
			ModuleReader reader,
			Module module,
			Class type,
			ref Raw.FieldDefStruct fieldDef
		)
		{
			var name = module.members.Strings[fieldDef.Name.Value];
			var access = GetAccessibility(reader, fieldDef.Flags);

			var field = new Field(name, access, parent: type);
			field.IsImplDetail = (fieldDef.Flags & Raw.FieldFlags.Impl) == Raw.FieldFlags.Impl;
			field.IsStatic = (fieldDef.Flags & Raw.FieldFlags.Instance) == 0;

			return field;
		}

		private static ClassConstant ReadClassConstant(
			ModuleReader reader,
			Module module,
			Class type,
			ref Raw.FieldDefStruct fieldDef
		)
		{
			var name = module.members.Strings[fieldDef.Name.Value];
			var access = GetAccessibility(reader, fieldDef.Flags);

			var valueStruct = reader.Deref(fieldDef.Value);

			bool success;
			Type valueTypeObject;
			var valueType = GetConstantType(
				reader,
				module,
				valueStruct.Type.Value,
				out valueTypeObject,
				out success
			);

			var value = success
				? ConstantValueFromRaw(valueType, module, valueTypeObject, unchecked((long)valueStruct.Value))
				: ConstantValue.Null;

			var constant = new ImportedClassConstant(name, access, parent: type, value: value);
			if (!success)
				reader.AddUnresolvedConstant(constant, valueStruct.Type.Value, unchecked((long)valueStruct.Value));

			return constant;
		}

		private static void ReadClassMethods(
			ModuleReader reader,
			Module module,
			Class type,
			int count,
			MetadataToken firstMethod
		)
		{
			if (count == 0)
				return;

			var methodDefs = module.members.MethodDefs;

			var address = reader.MethodsBase + Raw.FieldDefStruct.Size * firstMethod.Index;
			for (var i = 0; i < count; i++, address += Raw.MethodDefStruct.Size)
			{
				var token = unchecked(firstMethod.Value + (uint)i);

				var method = ReadSingleMethodDef(reader, module, type, address);

				type.ImportMethodGroup(method);
				if (method.Name == Constructor.InstanceCtorName)
					type.Constructors = method;

				var actualToken = methodDefs.Add(method);
				if (actualToken != token)
					ErrorWrongTokenValue(reader.FileName, actualToken, token);
			}
		}

		private static void ReadClassProperties(
			ModuleReader reader,
			Module module,
			Class type,
			int count,
			Raw.RvaToArray<Raw.PropertyDefStruct> rva
		)
		{
			if (count == 0)
				return;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.PropertyDefStruct.Size)
			{
				var propertyDef = reader.Read<Raw.PropertyDefStruct>(address);

				var name = module.members.Strings[propertyDef.Name.Value];

				if (propertyDef.Getter.Value == 0 && propertyDef.Setter.Value == 0)
					throw new ModuleLoadException(reader.FileName, "PropertyDef with no accessors.");

				MethodGroup getter = null, setter = null;
				if (propertyDef.Getter.Value != 0)
				{
					getter = module.members.MethodDefs[propertyDef.Getter.Value];
					if (getter.ParentAsClass != type)
						throw new ModuleLoadException(
							reader.FileName,
							"Property getter must refer to a method of the declaring class."
						);
				}
				if (propertyDef.Setter.Value != 0)
				{
					setter = module.members.MethodDefs[propertyDef.Setter.Value];
					if (setter.ParentAsClass != type)
						throw new ModuleLoadException(
							reader.FileName,
							"Property setter must refer to a method of the declaring class."
						);
				}

				if (name == Indexer.MemberName)
				{
					var indexer = CreateIndexer(reader, type, getter, setter);
					type.ImportIndexer(indexer);
				}
				else
				{
					var property = CreateProperty(reader, type, name, getter, setter);
					type.ImportProperty(property);
				}
			}
		}

		private static Property CreateProperty(
			ModuleReader reader,
			Class parent,
			string name,
			MethodGroup getter,
			MethodGroup setter
		)
		{
			ClassMemberMethod getterOverload = null, setterOverload = null;
			if (getter != null)
			{
				getterOverload = getter.FindOverload(Signature.Empty) as ClassMemberMethod;
				if (getterOverload == null)
					throw new ModuleLoadException(
						reader.FileName,
						"Could not find an overload for property getter."
					);
			}

			if (setter != null)
			{
				setterOverload = setter.FindOverload(Signature.OneRequired) as ClassMemberMethod;
				if (setterOverload == null)
					throw new ModuleLoadException(
						reader.FileName,
						"Could not find an overload for property setter."
					);
			}

			var property = new Property(name, parent);
			if (getterOverload != null)
				property.Getter = new PropertyAccessor(getterOverload, isSetter: false, isIndexer: false);
			if (setterOverload != null)
				property.Setter = new PropertyAccessor(setterOverload, isSetter: true, isIndexer: false);
			return property;
		}

		private static IndexerMember CreateIndexer(
			ModuleReader reader,
			Class parent,
			MethodGroup getters,
			MethodGroup setters
		)
		{
			var indexers = new Dictionary<int, Indexer>(
				Math.Max(
					getters != null ? getters.Count : 0,
					setters != null ? setters.Count : 0
				)
			);

			if (getters != null)
			{
				foreach (var overload in getters.OfType<ClassMemberMethod>())
				{
					var argCount = overload.Parameters.Length;
					Indexer indexer;
					if (!indexers.TryGetValue(argCount, out indexer))
						indexers.Add(argCount, indexer = new Indexer(parent, argCount));
					indexer.Getter = new IndexerAccessor(argCount, overload, isSetter: false);
				}
			}

			if (setters != null)
			{
				foreach (var overload in setters.OfType<ClassMemberMethod>())
				{
					// - 1 for the value parameter
					var argCount = overload.Parameters.Length - 1;
					Indexer indexer;
					if (!indexers.TryGetValue(argCount, out indexer))
						indexers.Add(argCount, indexer = new Indexer(parent, argCount));
					indexer.Setter = new IndexerAccessor(argCount, overload, isSetter: true);
				}
			}

			var indexerMember = new IndexerMember(parent);
			foreach (var indexer in indexers.Values)
				indexerMember.AddIndexer(indexer);

			return indexerMember;
		}

		private static void ReadClassOperators(
			ModuleReader reader,
			Module module,
			Class type,
			int count,
			Raw.RvaToArray<Raw.OperatorDefStruct> rva
		)
		{
			if (count == 0)
				return;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.OperatorDefStruct.Size)
			{
				var operatorDef = reader.Read<Raw.OperatorDefStruct>(address);

				var @operator = operatorDef.Operator;
				var method = module.members.MethodDefs[operatorDef.Method.Value];

				if (type.operators[(int)@operator] != null)
					throw new ModuleLoadException(reader.FileName, "Duplicate operator definition.");

				var overload = method.FindOverload(new Signature(GetArity(@operator), 0, Splat.None)) as ClassMemberMethod;
				if (overload == null)
					throw new ModuleLoadException(
						reader.FileName,
						"Could not find an overload for OperatorDef."
					);

				var operatorOverload = new OperatorOverload((int)@operator, type, overload);
				type.ImportOperatorOverload(operatorOverload);
			}
		}

		private static Type ReadEnumDef(
			ModuleReader reader,
			Module module,
			ref Raw.TypeDefStruct typeDef,
			uint enumToken,
			string fullName,
			Type baseType,
			bool isSet
		)
		{
			var result = new Enum(
				GetLastNameComponent(fullName),
				GetAccessibility(reader, typeDef.Flags),
				isSet: isSet,
				parent: module.explicitlyImported ? module.pool.Namespace : null
			);
			result.BaseType = baseType;

			ReadEnumFields(
				reader,
				module,
				result,
				typeDef.FieldCount,
				typeDef.FirstField,
				enumToken
			);

			// Add empty slots for the methods that would have belonged to this type,
			// so that later methods can receive correct token values.
			module.members.MethodDefs.AddEmpty(typeDef.MethodCount);

			return result;
		}

		private static void ReadEnumFields(
			ModuleReader reader,
			Module module,
			Enum type,
			int count,
			MetadataToken firstField,
			uint enumToken
		)
		{
			var address = reader.FieldsBase + Raw.FieldDefStruct.Size * firstField.Index;
			for (var i = 0; i < count; i++, address += Raw.FieldDefStruct.Size)
			{
				var fieldDef = reader.Read<Raw.FieldDefStruct>(address);
				ReadAnnotations(reader, fieldDef.Annotations);

				if (
					// Ignore non-public fields
					(fieldDef.Flags & Raw.FieldFlags.Public) == 0 ||
					// Ignore non-const fields
					(fieldDef.Flags & Raw.FieldFlags.HasValue) == 0
				)
					continue;

				// Ignore const fields that are not of the declaring enum type
				var valueStruct = reader.Deref(fieldDef.Value);
				if (valueStruct.Type.Value != enumToken)
					continue;

				var name = module.members.Strings[fieldDef.Name.Value];
				var field = new EnumField(
					name,
					unchecked((long)valueStruct.Value),
					type
				);
				type.DeclareField(field);
			}
		}

		private static void ReadFunctionDefs(
			ModuleReader reader,
			Module module,
			int count,
			Raw.RvaToArray<Raw.MethodDefStruct> rva
		)
		{
			// Global functions are only ever referred to in method bodies, which
			// we don't read, so if this module isn't explicitly imported, we don't
			// need to bother with global functions at all.
			if (count == 0 || !module.explicitlyImported)
				return;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.MethodDefStruct.Size)
			{
				// ReadSingleMethodDef adds the method to the namespace.
				ReadSingleMethodDef(reader, module, module.pool.Namespace, address);
			}
		}

		private static void ReadConstantDefs(
			ModuleReader reader,
			Module module,
			int count,
			Raw.RvaToArray<Raw.ConstantDefStruct> rva
		)
		{
			// Global constants are always inlined. If the target module is not explicitly imported,
			// then we don't need to care about the global constants.
			if (count == 0 || !module.explicitlyImported)
				return;

			var address = rva.Address;
			for (var i = 0; i < count; i++, address += Raw.ConstantDefStruct.Size)
			{
				var constantDef = reader.Read<Raw.ConstantDefStruct>(address);
				var access = GetAccessibility(reader, constantDef.Flags);
				if (access != AccessLevel.Public)
					continue;

				ReadAnnotations(reader, constantDef.Annotations);

				var fullName = module.members.Strings[constantDef.Name.Value];
				var valueStruct = reader.Deref(constantDef.Value);

				Type typeObject;
				var type = GetConstantType(reader, module, valueStruct.Type.Value, out typeObject);
				var value = ConstantValueFromRaw(type, module, typeObject, unchecked((long)valueStruct.Value));

				string name;
				var namespacePath = GetPathFromName(fullName, out name);
				var parentNs = module.pool.Namespace.GetNamespace(namespacePath, true);

				var constant = new GlobalConstant(name, value, access);

				parentNs.DeclareConstant(constant);
			}
		}

		private static MethodGroup ReadSingleMethodDef(
			ModuleReader reader,
			Module module,
			NamedMember parent,
			uint address
		)
		{
			var methodDef = reader.Read<Raw.MethodDefStruct>(address);

			var isGlobal = parent is Namespace;

			var name = module.members.Strings[methodDef.Name.Value];
			var access = GetAccessibility(reader, methodDef.Flags, isGlobal);

			if (methodDef.OverloadCount == 0)
				throw new ModuleLoadException(
					reader.FileName,
					string.Format("Method '{0}' has no overloads.", name)
				);

			if (isGlobal)
			{
				// Global function? The parent is the global declaration space.
				// Let's resolve the path to the correct namespace.
				var namespacePath = GetPathFromName(name, out name);
				parent = ((Namespace)parent).GetNamespace(namespacePath, true);
			}

			var method = new MethodGroup(name, parent, access);

			address = methodDef.Overloads.Address;
			for (var i = 0; i < methodDef.OverloadCount; i++, address += Raw.OverloadDefStruct.Size)
			{
				var overload = ReadOverloadDef(
					reader,
					module,
					name,
					access,
					methodDef.Flags,
					isGlobal,
					address
				);
				method.AddOverload(overload);
			}

			// This is an ugly, cheaty way of doing it, but since we already have
			// the correct parent namespace here in the case of a global function,
			// might as well add the method to it...
			// Note: we don't do this for internal global functions.
			if (isGlobal && access != AccessLevel.Internal)
				((Namespace)parent).ImportMethodGroup(method);

			method.Module = module;

			return method;
		}

		private static Method ReadOverloadDef(
			ModuleReader reader,
			Module module,
			string name,
			AccessLevel access,
			Raw.MethodFlags methodFlags,
			bool isGlobal,
			uint address
		)
		{
			var overloadDef = reader.Read<Raw.OverloadDefStruct>(address);
			ReadAnnotations(reader, overloadDef.Annotations);

			int optionalParamCount;
			var parameters = ReadParameters(
				reader,
				module,
				overloadDef.ParamCount,
				overloadDef.Params.Address,
				out optionalParamCount
			);

			var signature = new Signature(
				overloadDef.ParamCount,
				optionalParamCount,
				(overloadDef.Flags & Raw.OverloadFlags.Variadic) == Raw.OverloadFlags.Variadic ? Splat.End : Splat.None
			);
			var overload = isGlobal
				? new Method(null, name, access, null, signature)
				: new ClassMemberMethod(null, name, null, access, null, signature);

			overload.Parameters = parameters;
			overload.IsAbstract = (overloadDef.Flags & Raw.OverloadFlags.Abstract) == Raw.OverloadFlags.Abstract;
			overload.IsStatic = (methodFlags & Raw.MethodFlags.Instance) == 0;
			overload.IsImplDetail = (methodFlags & Raw.MethodFlags.Impl) == Raw.MethodFlags.Impl;
			overload.IsOverridable = (overloadDef.Flags & Raw.OverloadFlags.Virtual) == Raw.OverloadFlags.Virtual;
			overload.IsOverride = (overloadDef.Flags & Raw.OverloadFlags.Override) == Raw.OverloadFlags.Override;

			return overload;
		}

		private static Parameter[] ReadParameters(
			ModuleReader reader,
			Module module,
			int paramCount,
			uint address,
			out int optionalParamCount
		)
		{
			optionalParamCount = 0;
			if (paramCount == 0)
				return EmptyArrays.Parameters;

			var parameters = new Parameter[paramCount];

			for (var i = 0; i < paramCount; i++, address += Raw.ParameterStruct.Size)
			{
				var paramDef = reader.Read<Raw.ParameterStruct>(address);

				if ((paramDef.Flags & Raw.ParamFlags.Optional) == Raw.ParamFlags.Optional)
					optionalParamCount++;
				else if (optionalParamCount > 0)
					throw new ModuleLoadException(
						reader.FileName,
						"Required parameter cannot follow optional parameter."
					);

				var name = module.members.Strings[paramDef.Name.Value];
				parameters[i] = new Parameter(
					name,
					(paramDef.Flags & Raw.ParamFlags.ByRef) == Raw.ParamFlags.ByRef
				);
			}

			return parameters;
		}

		private static void ReadAnnotations(ModuleReader reader, uint annotations)
		{
			if (annotations != 0u)
				throw new ModuleLoadException(reader.FileName, "Annotations are not yet supported.");
		}

		private static void ResolveConstants(ModuleReader reader, Module module)
		{
			if (reader.UnresolvedConstants == null)
				return;

			foreach (var constant in reader.UnresolvedConstants)
			{
				Type typeObject;
				var type = GetConstantType(reader, module, constant.TypeId, out typeObject);
				var constantValue = ConstantValueFromRaw(type, module, typeObject, constant.Value);
				constant.Member.UpdateValue(constantValue);
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

		private static uint MakeToken(uint kindMask, int zeroBasedIndex)
		{
			return kindMask | unchecked((uint)zeroBasedIndex + 1);
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

		private static AccessLevel GetAccessibility(ModuleReader reader, Raw.TypeFlags flags)
		{
			const Raw.TypeFlags accessibility =
				Raw.TypeFlags.Public |
				Raw.TypeFlags.Internal;

			switch (flags & accessibility)
			{
				case Osprey.ModuleFile.Raw.TypeFlags.Public:
					return AccessLevel.Public;
				case Osprey.ModuleFile.Raw.TypeFlags.Internal:
					return AccessLevel.Internal;
				default:
					throw new ModuleLoadException(reader.FileName, "TypeDef has no declared accessibility.");
			}
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, Raw.FieldFlags flags)
		{
			const Raw.FieldFlags accessibility =
				Raw.FieldFlags.Public |
				Raw.FieldFlags.Internal |
				Raw.FieldFlags.Protected |
				Raw.FieldFlags.Private;

			switch (flags & accessibility)
			{
				case Osprey.ModuleFile.Raw.FieldFlags.Public:
					return AccessLevel.Public;
				case Osprey.ModuleFile.Raw.FieldFlags.Internal:
					return AccessLevel.Internal;
				case Osprey.ModuleFile.Raw.FieldFlags.Protected:
					return AccessLevel.Protected;
				case Osprey.ModuleFile.Raw.FieldFlags.Private:
					return AccessLevel.Private;
				default:
					throw new ModuleLoadException(reader.FileName, "FieldDef has no declared accessibility.");
			}
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, Raw.MethodFlags flags, bool isGlobal)
		{
			const Raw.MethodFlags accessibility =
				Raw.MethodFlags.Public |
				Raw.MethodFlags.Internal |
				Raw.MethodFlags.Protected |
				Raw.MethodFlags.Private;

			switch (flags & accessibility)
			{
				case Osprey.ModuleFile.Raw.MethodFlags.Public:
					return AccessLevel.Public;
				case Osprey.ModuleFile.Raw.MethodFlags.Internal:
					return AccessLevel.Internal;
				case Osprey.ModuleFile.Raw.MethodFlags.Protected:
					return AccessLevel.Protected;
				case Osprey.ModuleFile.Raw.MethodFlags.Private:
					return AccessLevel.Private;
				default:
					throw new ModuleLoadException(reader.FileName, "MethodDef or FunctionDef has no declared accessibility.");
			}
		}

		private static AccessLevel GetAccessibility(ModuleReader reader, Raw.ConstantFlags flags)
		{
			const Raw.ConstantFlags accessibility =
				Raw.ConstantFlags.Public |
				Raw.ConstantFlags.Internal;

			switch (flags & accessibility)
			{
				case Osprey.ModuleFile.Raw.ConstantFlags.Public:
					return AccessLevel.Public;
				case Osprey.ModuleFile.Raw.ConstantFlags.Internal:
					return AccessLevel.Internal;
				default:
					throw new ModuleLoadException(reader.FileName, "ConstantDef has no declared accessibility.");
			}
		}

		private static int GetArity(Raw.Operator op)
		{
			switch (op)
			{
				case Raw.Operator.Plus:
				case Raw.Operator.Negate:
				case Raw.Operator.Not:
					return 1;
				default:
					return 2;
			}
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

		private static string GetLastNameComponent(string fullName)
		{
			var lastDot = fullName.LastIndexOf('.');
			if (lastDot != -1)
				return fullName.Substring(lastDot + 1);
			else
				return fullName;
		}

		private static void ErrorWrongTokenValue(string fileName, uint actualToken, uint expectedToken)
		{
			throw new ModuleLoadException(
				fileName,
				string.Format("Incorrect member token (got {0:X8}, expected {1:X8})", actualToken, expectedToken)
			);
		}
	}
}