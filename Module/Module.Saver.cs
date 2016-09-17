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

			throw new NotImplementedException();
		}
	}
}