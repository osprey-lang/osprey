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
		public long Save(string targetPath)
		{
			if (imported)
				throw new InvalidOperationException("Cannot save imported module.");

			var writer = new ModuleWriter();
			writer.AddBasicModuleData(this);

			WriteReferences(writer);
			WriteDefinitions(writer);

			throw new NotImplementedException();
		}

		private void WriteReferences(ModuleWriter writer)
		{
			foreach (var module in members.ModuleRefs.Values)
				writer.CreateModuleRef(module);

			foreach (var type in members.TypeRefs.Values)
				writer.CreateTypeRef(type);

			foreach (var field in members.FieldRefs.Values)
				writer.CreateFieldRef(field);

			foreach (var method in members.MethodRefs.Values)
				writer.CreateMethodRef(method);

			foreach (var function in members.GlobalFuncRefs.Values)
				writer.CreateFunctionRef(function);
		}

		private void WriteDefinitions(ModuleWriter writer)
		{
			foreach (var type in members.TypeDefs.Values)
				writer.CreateTypeDef(type);

			foreach (var function in members.GlobalFuncDefs.Values)
				writer.CreateMethodDef(function);

			foreach (var constant in members.GlobalConstDefs.Values)
				writer.CreateConstantDef(constant);
		}
	}
}