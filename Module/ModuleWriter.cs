using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Osprey.ModuleFile;

namespace Osprey
{
	public class ModuleWriter
	{
		public ModuleWriter(Module module)
			: this(module, null)
		{ }

		public ModuleWriter(Module module, IFileObjectFactory fileObjectFactory)
		{
			this.module = module;
			this.fileObjectFactory = fileObjectFactory ?? new FileObjectFactory(this);

			allSections = new FileObjectArray<FileSection>(null, 5)
			{
				stringData,
				metadata,
				references,
				definitions,
				methodBodies,
			};
		}

		private Module module;
		private IFileObjectFactory fileObjectFactory;

		private StringDataSection stringData = new StringDataSection();
		private MetadataSection metadata = new MetadataSection();
		private ReferencesSection references = new ReferencesSection();
		private DefinitionsSection definitions = new DefinitionsSection();
		private MethodBodySection methodBodies = new MethodBodySection();
		private FileObjectArray<FileSection> allSections;
	}
}