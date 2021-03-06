﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	/// <summary>
	/// Represents a token type. All TokenType values have a category mask, which represents
	/// the token's category, e.g. keyword or punctuation. Some may contain information in
	/// addition to this mask, such as the precise keyword or the particular punctuation token.
	/// A TokenType may belong to multiple categories. E.g.: 'null', 'true' and 'false' all
	/// belong to <see cref="TokenType.Keyword"/>, but also to <see cref="TokenType.Literal"/>.
	/// Note, however, that keywords do NOT have the category flag <see cref="TokenType.Identifier"/>.
	/// </summary>
	[Flags]
	public enum TokenType
	{
		/// <summary>The token is invalid or unknown.</summary>
		Invalid = 0,
		/// <summary>The token is invalid or unknown (synonymous with <see cref="TokenType.Invalid"/></summary>
		None = 0,
		/// <summary>The end of the file.</summary>
		EOF = 1,

		/// <summary>A mask for extracting the category of a <see cref="TokenType"/>.</summary>
		CategoryMask = 0x7fff0000,
		/// <summary>A mask for extracting the value of a <see cref="TokenType"/>.</summary>
		ValueMask = 0x0000ffff,

		#region Categories
		/// <summary>The token is an identifier.</summary>
		IdentifierCategory = 0x00010000,
		/// <summary>The token is a keyword.</summary>
		Keyword = 0x00020000,
		/// <summary>The token is a literal value.</summary>
		Literal = 0x00040000,
		/// <summary>The token is a punctuation token.</summary>
		Punctuation = 0x00080000,
		/// <summary>A comment.</summary>
		Comment = 0x00100000,
		/// <summary>The token is a compound assignment operator.</summary>
		CompoundAssign = 0x00200000,
		/// <summary>The token is a member modifier, such as public, protected, sealed, virtual, etc.</summary>
		MemberModifier = 0x00400000,
		/// <summary>The token is an overloadable binary operator.</summary>
		OverloadableBinaryOperator = 0x00800000,
		/// <summary>The token matches the lambda-operator production (it can immediately follow an <code>@</code>)</summary>
		LambdaOperator = 0x01000000,
		#endregion

		// values
		Identifier = IdentifierCategory | 1,

		#region Keywords
		/// <summary>abstract</summary>
		Abstract = 1 | Keyword | MemberModifier,
		/// <summary>and</summary>
		And = 2 | Keyword | LambdaOperator,
		/// <summary>async (unused)</summary>
		Async = 3 | Keyword,
		/// <summary>base</summary>
		Base = 4 | Keyword,
		/// <summary>break</summary>
		Break = 5 | Keyword,
		/// <summary>catch</summary>
		Catch = 6 | Keyword,
		/// <summary>class</summary>
		Class = 7 | Keyword,
		/// <summary>const</summary>
		Const = 8 | Keyword,
		/// <summary>do</summary>
		Do = 9 | Keyword,
		/// <summary>else</summary>
		Else = 10 | Keyword,
		/// <summary>enum</summary>
		Enum = 11 | Keyword,
		/// <summary>false</summary>
		False = 12 | Keyword | Literal,
		/// <summary>finally</summary>
		Finally = 13 | Keyword,
		/// <summary>for</summary>
		For = 14 | Keyword,
		/// <summary>function</summary>
		Function = 15 | Keyword,
		/// <summary>global</summary>
		Global = 16 | Keyword,
		/// <summary>if</summary>
		If = 17 | Keyword,
		/// <summary>in</summary>
		In = 18 | Keyword,
		/// <summary>inheritable</summary>
		Inheritable = 19 | Keyword | MemberModifier,
		/// <summary>internal</summary>
		Internal = 20 | Keyword | MemberModifier,
		/// <summary>is</summary>
		Is = 21 | Keyword,
		/// <summary>iter</summary>
		Iter = 22 | Keyword,
		/// <summary>let</summary>
		Let = 23 | Keyword,
		/// <summary>namespace</summary>
		Namespace = 24 | Keyword,
		/// <summary>new</summary>
		New = 25 | Keyword,
		/// <summary>next</summary>
		Next = 26 | Keyword,
		/// <summary>not</summary>
		Not = 27 | Keyword | LambdaOperator,
		/// <summary>null</summary>
		Null = 28 | Keyword | Literal,
		/// <summary>operator</summary>
		Operator = 29 | Keyword,
		/// <summary>or</summary>
		Or = 30 | Keyword | LambdaOperator,
		/// <summary>overridable</summary>
		Overridable = 31 | Keyword | MemberModifier,
		/// <summary>override</summary>
		Override = 32 | Keyword | MemberModifier,
		/// <summary>private</summary>
		Private = 33 | Keyword | MemberModifier,
		/// <summary>protected</summary>
		Protected = 34 | Keyword | MemberModifier,
		/// <summary>public</summary>
		Public = 35 | Keyword | MemberModifier,
		/// <summary>refeq</summary>
		Refeq = 36 | Keyword,
		/// <summary>return</summary>
		Return = 37 | Keyword,
		/// <summary>static</summary>
		Static = 38 | Keyword | MemberModifier,
		/// <summary>this</summary>
		This = 39 | Keyword,
		/// <summary>throw</summary>
		Throw = 40 | Keyword,
		/// <summary>true</summary>
		True = 41 | Keyword | Literal,
		/// <summary>try</summary>
		Try = 42 | Keyword,
		/// <summary>typeof</summary>
		Typeof = 43 | Keyword,
		/// <summary>use</summary>
		Use = 44 | Keyword,
		/// <summary>var</summary>
		Var = 45 | Keyword,
		/// <summary>while</summary>
		While = 46 | Keyword,
		/// <summary>xor</summary>
		Xor = 47 | Keyword | LambdaOperator,
		/// <summary>yield</summary>
		Yield = 48 | Keyword,
		/// <summary>ref</summary>
		Ref = 49 | Keyword,
		/// <summary>with</summary>
		With = 50 | Keyword,
		#endregion

		#region Literals
		/// <summary>An integer literal (signed or unsigned).</summary>
		Integer = 1 | Literal,
		/// <summary>A real (floating-point) literal.</summary>
		Real = 2 | Literal,
		/// <summary>A string literal.</summary>
		String = 3 | Literal,
		/// <summary>A character literal.</summary>
		Character = 4 | Literal,
		#endregion

		#region Punctuation
		/// <summary>{</summary>
		CurlyOpen = 1 | Punctuation,
		/// <summary>}</summary>
		CurlyClose = 2 | Punctuation,
		/// <summary>[</summary>
		SquareOpen = 3 | Punctuation,
		/// <summary>]</summary>
		SquareClose = 4 | Punctuation,
		/// <summary>(</summary>
		ParenOpen = 5 | Punctuation,
		/// <summary>)</summary>
		ParenClose = 6 | Punctuation,
		/// <summary>.</summary>
		Dot = 7 | Punctuation,
		/// <summary>,</summary>
		Comma = 8 | Punctuation,
		/// <summary>:</summary>
		Colon = 9 | Punctuation,
		/// <summary>;</summary>
		Semicolon = 10 | Punctuation,
		/// <summary>~</summary>
		Tilde = 11 | Punctuation | LambdaOperator,
		/// <summary>&lt;</summary>
		Less = 12 | Punctuation | LambdaOperator,
		/// <summary>&lt;=</summary>
		LessEqual = 13 | Punctuation | LambdaOperator,
		/// <summary>&gt;</summary>
		Greater = 14 | Punctuation | LambdaOperator,
		/// <summary>&gt;=</summary>
		GreaterEqual = 15 | Punctuation | LambdaOperator,
		/// <summary>==</summary>
		DoubleEqual = 16 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>!=</summary>
		NotEqual = 17 | Punctuation | LambdaOperator,
		/// <summary>?</summary>
		Question = 18 | Punctuation,
		/// <summary>??</summary>
		NullCoalescing = 19 | Punctuation,
		/// <summary>?!</summary>
		NullOr = 20 | Punctuation,
		/// <summary>-&gt;</summary>
		FuncApplication = 21 | Punctuation | LambdaOperator,
		/// <summary>+</summary>
		Plus = 22 | Punctuation | LambdaOperator,
		/// <summary>-</summary>
		Minus = 23 | Punctuation | LambdaOperator,
		/// <summary>|</summary>
		Pipe = 24 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>*</summary>
		Multiply = 25 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>/</summary>
		Divide = 26 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>%</summary>
		Mod = 27 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>&amp;</summary>
		Ampersand = 28 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>^</summary>
		Caret = 29 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>::</summary>
		Concatenation = 30 | Punctuation | LambdaOperator,
		/// <summary>&lt;&lt;</summary>
		ShiftLeft = 31 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>&gt;&gt;</summary>
		ShiftRight = 32 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>**</summary>
		Power = 33 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>=</summary>
		Assign = 34 | Punctuation,
		/// <summary>+=</summary>
		PlusAssign = 35 | Punctuation | CompoundAssign,
		/// <summary>-=</summary>
		MinusAssign = 36 | Punctuation | CompoundAssign,
		/// <summary>|=</summary>
		PipeAssign = 37 | Punctuation | CompoundAssign,
		/// <summary>*=</summary>
		MulAssign = 38 | Punctuation | CompoundAssign,
		/// <summary>/=</summary>
		DivAssign = 39 | Punctuation | CompoundAssign,
		/// <summary>%=</summary>
		ModAssign = 40 | Punctuation | CompoundAssign,
		/// <summary>&amp;=</summary>
		AmpAssign = 41 | Punctuation | CompoundAssign,
		/// <summary>^=</summary>
		CaretAssign = 42 | Punctuation | CompoundAssign,
		/// <summary>::=</summary>
		ConcatAssign = 43 | Punctuation | CompoundAssign,
		/// <summary>&lt;&lt;=</summary>
		ShiftLeftAssign = 44 | Punctuation | CompoundAssign,
		/// <summary>&gt;gt;=</summary>
		ShiftRightAssign = 45 | Punctuation | CompoundAssign,
		/// <summary>**=</summary>
		PowerAssign = 46 | Punctuation | CompoundAssign,
		/// <summary>#=</summary>
		At = 47 | Punctuation,
		/// <summary>...</summary>
		Splat = 48 | Punctuation,
		/// <summary>?.</summary>
		SafeAccess = 49 | Punctuation,
		/// <summary>&lt;=&gt;</summary>
		Compare = 50 | Punctuation | OverloadableBinaryOperator | LambdaOperator,
		/// <summary>?(</summary>
		ParenOpenSafe = 51 | Punctuation,
		/// <summary>?[</summary>
		SquareOpenSafe = 52 | Punctuation,
		/// <summary>=&gt;</summary>
		FatArrow = 53 | Punctuation,
		#endregion
	}
}
