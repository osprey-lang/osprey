using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Osprey.Members;
using Osprey.Instructions;

namespace Osprey.Nodes
{
	// This file contains compiler extensions.

	/// <summary>
	/// Represents an __extern body. This is only applicable to members with bodies,
	/// such as property accessors, constructors and methods.
	/// </summary>
	public sealed class ExternBody : Block
	{
		public ExternBody(StringLiteral entryPoint, Expression locals, Expression maxStack)
			: base((List<Statement>)null)
		{
			EntryPoint = entryPoint;
			Locals = locals;
			MaxStack = maxStack;
		}

		/// <summary>
		/// The name of the external method that implements the member.
		/// </summary>
		public StringLiteral EntryPoint;
		/// <summary>
		/// The number of managed locals the method uses.
		/// </summary>
		public Expression Locals;
		/// <summary>
		/// The maximum number of stack slots used by the method.
		/// </summary>
		public Expression MaxStack;

		public override string ToString(int indent)
		{
			string localString = Locals != null ? ", locals=" + Locals.ToString(indent + 1) : "";

			string stackString = MaxStack != null ? ", stack=" + MaxStack.ToString(indent + 1) : "";

			return string.Format("__extern({0}{1}{2});",
				EntryPoint.ToString(indent + 1), localString, stackString);
		}

		// do nothing with the contents of the body; Statements is null

		public override void FoldConstant()
		{
			Locals = Locals.FoldConstant();
			if (!(Locals is ConstantExpression) || ((ConstantExpression)Locals).Value.Type != ConstantValueType.Int)
				throw new CompileTimeException(Locals, "The __extern 'locals' parameter must be a constant expression of type Int.");

			MaxStack = MaxStack.FoldConstant();
			if (!(MaxStack is ConstantExpression) || ((ConstantExpression)MaxStack).Value.Type != ConstantValueType.Int)
				throw new CompileTimeException(MaxStack, "The __extern 'stack' parameter must be a constant expression of type Int.");
		}

		public override void ResolveNames(IDeclarationSpace context, FileNamespace document, bool reachable)
		{
			Locals = Locals.ResolveNames(context, document, false, false);
			MaxStack = MaxStack.ResolveNames(context, document, false, false);
		}

		public override void DeclareNames(BlockSpace parent) { }

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			throw new InvalidOperationException("You are not supposed to call .Compile on extern bodies!");
		}
	}

	/// <summary>
	/// Represents a __named_const() expression, which refers to a constant from a pool
	/// of named constants.
	/// </summary>
	public sealed class NamedConstant : ConstantExpression
	{
		public NamedConstant(string scope, string name)
		{
			Scope = scope;
			Name = name;
			this.Value = GetConstant(scope, name);
		}

		public string Scope, Name;

		public override string ToString(int indent)
		{
			return string.Format("__named_const({0}: {1})", Scope, Name);
		}

		private static Dictionary<string, long> intConsts = new Dictionary<string, long>
		{
			{"min", long.MinValue},
			{"max", long.MaxValue},
		};

		private static Dictionary<string, ulong> uintConsts = new Dictionary<string, ulong>
		{
			{"min", ulong.MinValue},
			{"max", ulong.MaxValue},
		};

		private static Dictionary<string, double> realConsts = new Dictionary<string, double>
		{
			{"min", double.MinValue},
			{"max", double.MaxValue},
			{"epsilon", double.Epsilon},
			{"NaN", double.NaN},
			{"inf", double.PositiveInfinity},
			{"pi", Math.PI},
			{"e", Math.E},
			{"sqrt2", Math.Sqrt(2.0)},
			{"phi", (1.0 + Math.Sqrt(5.0)) / 2.0}
		};

		public static ConstantValue GetConstant(string scope, string name)
		{
			switch (scope)
			{
				case "int":
					if (!intConsts.ContainsKey(name))
						break;
					return ConstantValue.CreateInt(intConsts[name]);
				case "uint":
					if (!uintConsts.ContainsKey(name))
						break;
					return ConstantValue.CreateUInt(uintConsts[name]);
				case "real":
					if (!realConsts.ContainsKey(name))
						break;
					return ConstantValue.CreateReal(realConsts[name]);
				default:
					throw new ArgumentException("Invalid named constant scope: " + scope);
			}

			throw new ArgumentException(string.Format("Invalid constant name in scope '{0}': {1}", scope, name));
		}
	}

	/// <summary>
	/// Represents the __get_argc extension keyword, which compiles directly to the ldargc instruction.
	/// </summary>
	public sealed class GetArgumentCount : Expression
	{
		public GetArgumentCount()
		{ }

		public override bool IsTypeKnown(Compiler compiler) { return true; }

		public override Members.Type GetKnownType(Compiler compiler)
		{
			return compiler.IntType;
		}

		public override string ToString(int indent)
		{
			return "__get_argc";
		}

		public override Expression ResolveNames(IDeclarationSpace context, FileNamespace document)
		{
			if (!(context is BlockSpace))
				throw new CompileTimeException(this, "'__get_argc' may only appear inside a method body.");

			// __get_argc becomes constant if the containing method has no optional parameters and is not variadic.
			// Instead of storing the arg count somewhere and then returning it during the FoldConstant step, we
			// return a ConstantExpression here right away.

			var signature = ((BlockSpace)context).Method.Signature;
			if (signature.OptionalParameterCount == 0 && signature.Splat == Splat.None)
				return new ConstantExpression(ConstantValue.CreateInt(signature.ParameterCount))
				{
					StartIndex = this.StartIndex,
					EndIndex = this.EndIndex,
				};

			return this;
		}

		public override void Compile(Compiler compiler, MethodBuilder method)
		{
			method.Append(new SimpleInstruction(Opcode.Ldargc));
		}
	}
}