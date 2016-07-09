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

			// TODO
			throw new NotImplementedException();
		}
	}
}