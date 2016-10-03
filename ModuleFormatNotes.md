Where the module specification uses an RVA to an array, we generally want to put the array as close as possible to the thing that uses it, which typically means right after it in byte order. However, when the thing that contains the RVA-to-array is a fixed-size struct that is used inside other arrays, we cannot do so. Consider:

    struct Outer:
      int fooCount
      Rva<Foo[fooCount]> foos

    struct Foo:
      int barCount
      Rva<Bar[barCount]> bars

    struct Bar:
      (some data)

Each `Foo` owns one or more `Bar`s. If we were to lay out the `Bar`s such that they follow each corresponding `Foo`, we would effectively force `Foo` to be variable-size. That means `Outer.foos` could not exist, because `Rva<Foo[]>` requires `Foo` to have a fixed size.

To solve this, one can attempt to move the `Bar`s up to the nearest parent that is not strictly required to be fixed-size (e.g. because it's only referred to in "simple" RVAs), or one can make them "global"; that is, have a section for them at the `ModuleWriter` level. The general solution used here is to just make them global. In some cases, such as try blocks, which we can trivially associate with a single method, we keep the data as close to the logical owner as possible.

That, essentially, is why `ModuleWriter` has so many fields and is why there are so *few* arrays in the `FileObject` types.

The illustration below shows the relation between objects. Types marked with `~` are variable-size; `->` means "points to" (i.e. RVA); `[T]` means array of `T`; and the type name alone means the value is inlined. Not all of the types below are represented by `FileObject`s. If a type is not listed, it has no special references. Annotations are also not listed, because they are not supported.

    ModuleHeader
      -> String~  -- x2: name and nativeLib
      -> StringTableHeader
      -> RefTableHeader
      -> StringMapHeader~
      -> [TypeDef]
      -> [FieldDef]
      -> [MethodDef]
      -> [ConstantDef]

    StringTableHeader
      [-> String~]

    RefTableHeader
      -> [ModuleRef]
      -> [TypeRef]
      -> [FieldRef]
      -> [MethodRef]
      -> [FunctionRef]

    StringMapHeader~
      [StringMapEntry]

    StringMapEntry
      -> String~  -- x2: key and value

    TypeDef
      -> ByteString~
      -> [FieldDef]  -- indirectly; start token + count
      -> [MethodDef]  -- indirectly; start token + count
      -> [PropertyDef]
      -> [OperatorDef]

    FieldDef
      -> ConstantValue

    MethodDef
      -> [OverloadDef]

    OverloadDef
      -> [Parameter]
      -> MethodDef~
      -> MethodHeader~
      -> NativeMethodHeader~

    MethodHeader~
      -> [TryBlock]
      MethodBody~

    MethodBody~
      [byte]  -- method body data

    NativeMethodHeader~
      ByteString~

    TryBlock
      CatchClauses

    CatchClauses
      -> [CatchClause]

From this we get the following file layout:

    ModuleHeader
    RefTableHeader
    string data:
      StringTableHeader
        [-> String]  -- RVAs to all the strings
      [String]  -- all string data
      [ByteString]  -- all byte string data referenced indirectly (these have no tokens)
    metadata:
      StringMapHeader
        [StringMapEntry]  -- each StringMapEntry points into the string data array
    references:
      [ModuleRef]
      [TypeRef]
      [FieldRef]
      [MethodRef]
      [FunctionRef]
    definitions:
      [TypeDef]
      [PropertyDef]
      [OperatorDef]
      [FieldDef]
      [MethodDef]  -- class methods
      [MethodDef]  -- global functions
      [OverloadDef]
      [Parameter]
    constant pool:
      [ConstantValue]  -- each value grouped by type and value
    [
      -- It is no problem keeping this array of variable-size data, because all the entries
      -- are referred to individually. In other words, there are no arrays of MethodHeaders,
      -- MethodBodys or NativeMethodHeaders.
      method data, union of:
        MethodHeader
          MethodBody
            [byte]
          [TryBlock]
          [CatchClause]
        MethodBody
          [byte]
        NativeMethodHeader
          ByteString
    ]
