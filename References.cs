using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;

namespace Osprey
{
	public abstract class MemberRef
	{
		protected MemberRef(uint id/*, Module module*/)
		{
			this.id = id;
			//this.module = module;
		}

		private uint id;
		/// <summary>
		/// Gets the ID of the reference.
		/// </summary>
		public uint Id { get { return id; } }

		//private Module module;
		// /// <summary>
		// /// Gets the module that declares the member.
		// /// </summary>
		//public Module Module { get { return module; } }
	}

	// TODO: class StringRef
	public sealed class StringRef : MemberRef
	{
		internal StringRef(uint id, string value)
			: base(id)
		{
			this.value = value;
		}

		private string value;
		/// <summary>
		/// Gets the value referenced.
		/// </summary>
		public string Value { get { return value; } }
	}

	// TODO: class TypeRef
	public sealed class TypeRef : MemberRef
	{
		internal TypeRef(uint id)
			: base(id)
		{ }
	}

	// TODO: class MethodRef
	public sealed class MethodRef : MemberRef
	{
		internal MethodRef(uint id)
			: base(id)
		{ }
	}

	// TODO: class ModuleRef
	public sealed class ModuleRef : MemberRef
	{
		internal ModuleRef(uint id)
			: base(id)
		{ }
	}
}