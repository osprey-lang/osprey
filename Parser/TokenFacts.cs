using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Osprey
{
	internal class TokenFacts
	{
		internal static readonly Dictionary<string, TokenType> IdentToKeyword = new Dictionary<string, TokenType>
		{
			{"abstract", TokenType.Abstract},
			{"and", TokenType.And},
			{"async", TokenType.Async},
			{"base", TokenType.Base},
			{"break", TokenType.Break},
			{"catch", TokenType.Catch},
			{"class", TokenType.Class},
			{"const", TokenType.Const},
			{"do", TokenType.Do},
			{"else", TokenType.Else},
			{"enum", TokenType.Enum},
			{"false", TokenType.False},
			{"finally", TokenType.Finally},
			{"for", TokenType.For},
			{"function", TokenType.Function},
			{"global", TokenType.Global},
			{"if", TokenType.If},
			{"in", TokenType.In},
			{"inheritable", TokenType.Inheritable},
			{"is", TokenType.Is},
			{"iter", TokenType.Iter},
			{"namespace", TokenType.Namespace},
			{"new", TokenType.New},
			{"next", TokenType.Next},
			{"not", TokenType.Not},
			{"null", TokenType.Null},
			{"operator", TokenType.Operator},
			{"or", TokenType.Or},
			{"overridable", TokenType.Overridable},
			{"override", TokenType.Override},
			{"private", TokenType.Private},
			{"protected", TokenType.Protected},
			{"public", TokenType.Public},
			{"ref", TokenType.Ref},
			{"refeq", TokenType.Refeq},
			{"return", TokenType.Return},
			{"static", TokenType.Static},
			{"this", TokenType.This},
			{"throw", TokenType.Throw},
			{"true", TokenType.True},
			{"try", TokenType.Try},
			{"typeof", TokenType.Typeof},
			{"use", TokenType.Use},
			{"var", TokenType.Var},
			{"while", TokenType.While},
			{"with", TokenType.With},
			{"xor", TokenType.Xor},
			{"yield", TokenType.Yield},
		};

		internal static readonly Dictionary<string, ContextualType> IdentToContextual = new Dictionary<string, ContextualType>
		{
			{"get", ContextualType.Get},
			{"set", ContextualType.Set},
			{"to", ContextualType.To},
			{"where", ContextualType.Where},
			{"version", ContextualType.Version},
			{"__primitive", ContextualType.Primitive},
			{"__init_type", ContextualType.InitType},
			{"__extern", ContextualType.Extern},
			{"__named_const", ContextualType.NamedConst},
			{"__get_argc", ContextualType.GetArgc},
		};

		internal static readonly Dictionary<string, TokenType> PunctToType = new Dictionary<string, TokenType>
		{
			{"{", TokenType.CurlyOpen},
			{"}", TokenType.CurlyClose},
			{"[", TokenType.SquareOpen},
			{"]", TokenType.SquareClose},
			{"(", TokenType.ParenOpen},
			{")", TokenType.ParenClose},
			{".", TokenType.Dot},
			{",", TokenType.Comma},
			{":", TokenType.Colon},
			{";", TokenType.Semicolon},
			{"~", TokenType.Tilde},
			{"<", TokenType.Less},
			{"<=", TokenType.LessEqual},
			{">", TokenType.Greater},
			{">=", TokenType.GreaterEqual},
			{"==", TokenType.DoubleEqual},
			{"!=", TokenType.NotEqual},
			{"?", TokenType.Question},
			{"??", TokenType.NullCoalescing},
			{"?!", TokenType.NullOr},
			{"->", TokenType.FuncApplication},
			{"+", TokenType.Plus},
			{"-", TokenType.Minus},
			{"|", TokenType.Pipe},
			{"*", TokenType.Multiply},
			{"/", TokenType.Divide},
			{"%", TokenType.Mod},
			{"&", TokenType.Ampersand},
			{"^", TokenType.Caret},
			{"::", TokenType.Concatenation},
			{"<<", TokenType.ShiftLeft},
			{">>", TokenType.ShiftRight},
			{"**", TokenType.Power},
			{"=", TokenType.Assign},
			{"+=", TokenType.PlusAssign},
			{"-=", TokenType.MinusAssign},
			{"|=", TokenType.PipeAssign},
			{"*=", TokenType.MulAssign},
			{"/=", TokenType.DivAssign},
			{"%=", TokenType.ModAssign},
			{"&=", TokenType.AmpAssign},
			{"^=", TokenType.CaretAssign},
			{"::=", TokenType.ConcatAssign},
			{"<<=", TokenType.ShiftLeftAssign},
			{">>=", TokenType.ShiftRightAssign},
			{"**=", TokenType.PowerAssign},
			{"@", TokenType.At},
			{"...", TokenType.Splat},
			{"?.", TokenType.SafeAccess},
			{"<=>", TokenType.Compare},
			{"?(", TokenType.ParenOpenSafe},
			{"?[", TokenType.SquareOpenSafe},
		};

		// Assigned by static constructor
		internal static readonly Dictionary<TokenType, string> KeywordToString;

		// Assigned by static constructor
		internal static readonly Dictionary<TokenType, string> PunctToString;

		// Assigned by static constructor
		internal static readonly int LongestKeywordLength;

		// Assigned by static constructor
		internal static readonly int LongestContextualLength;

		static TokenFacts()
		{
			var longestKeywordLength = 0;

			KeywordToString = new Dictionary<TokenType, string>(IdentToKeyword.Count);
			foreach (var kvp in IdentToKeyword)
			{
				KeywordToString.Add(kvp.Value, kvp.Key);
				longestKeywordLength = Math.Max(longestKeywordLength, kvp.Key.Length);
			}

			LongestKeywordLength = longestKeywordLength;

			PunctToString = new Dictionary<TokenType, string>(PunctToType.Count);
			foreach (var kvp in PunctToType)
				PunctToString.Add(kvp.Value, kvp.Key);

			LongestContextualLength = IdentToContextual.Keys
				.Select(k => k.Length)
				.Aggregate(0, Math.Max);
		}
	}
}