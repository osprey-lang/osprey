# Simplified Tree

The following transformations are performed when `ParseFlags.SimplifiedTree` is specified:

1. Enum fields get assigned a value based on an internal counter. That is, all `EnumValue` instances get a real `Expression` as their value, which is a `ConstantExpression` of type Int when the counter is used. (Note: the counter _does_ enumerate enum sets correctly! Otherwise it'd be kinda useless.)

2. Bodies of the form `= expr`, used in property/indexer getters and lambda expressions, get transformed into `{ return expr; }`, guaranteeing that all such constructs have a `Block` as their body.

3. `EmbeddedStatement`s in control statement bodies (of ifs, elses, fors, etc.) get replaced by blocks containing the inner statement. Blocks remain unchanged.

4. Binary and unary operator expressions involving values known to be constant at parse-time (i.e. literals) are reduced to `ConstantExpression`s as far as possible. This reduction _may_ throw, if you do silly things like divide an integer by 0 or multiply huge numbers, but that's okay; it's not supposed to compile anyway.

5. The step value in a range expression gets initialized to 1 (Int), if missing.

6. The locals and stack parameters of an `__extern` declaration get initialised to 0 and 8, respectively, if missing.

7. If a `return` or `yield` has multiple return values, they're wrapped in a `ListLiteralExpression`.

8. Binary operator expressions of the form `a != b` get turned into `not (a == b)`.

Notably, parenthesized expressions are _not_ stripped. This is partly because it is invalid for the inner expression in a parenthesized expression to refer to a namespace or type. In other words, the following should not compile:

	class Foo { static value; }
	(Foo).value = 10;

even though `Foo` declares a field `value`. If the parenthesized expression were stripped out, we would not be able to detect this situation, as the compiler would see it as `Foo.value`, which is valid. `ParenthesizedExpression.ResolveNames` performs this check, and never returns a `ParenthesizedExpression`.

The other reason has to do with lvalues. A parenthesized expression can never be assigned to, e.g. the following is wrong:

	(a.b.c) = 123;

but if we stripped out the parenthesized expression, it would compile just fine.