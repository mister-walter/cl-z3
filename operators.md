# Documentation

## Sorts
- `:bool`: Boolean
- `:int`: arbitrary-precision integer
- `(:bv <n>)`: a bitvector of length `n`
- `(:seq <ty>)`: a sequence with elements of type `ty`
- `(:set <ty>)`: a set with elements of type `ty`
- `:string`: shorthand for `(:seq (:bv 8))` (a sequence of 8-bit values, e.g. ASCII characters)
- `(:regex <seq-ty>)`: a regular expression over the sequence type `seq-ty`
- `(:array <dom> <rng>)`: an array with domain `dom` and range `rng`
- `(:fn (<dom1>, ..., <domn>) <rng>)`: a function with domain `dom1 x ... x domn` and range `rng`

### Enums

An enum sort consists of a finite set of elements, specified when the
enum is registered.

The following line defines an enum sort with the name `:bar` and the elements `a`, `b`, and `c`.
`(register-enum-sort :bar (a b c))`

A constant enum element can be expressed using `(enumval <ty> <el>)`.
For example, the following statement asserts that `q` is equal to the
`:bar` element `a`.

```
(z3-assert (q :bar)
           (= q (enumval :bar a)))
```

### Named tuples

A named tuple sort consists of a set of fields, each of which has a
name and a sort.

The following line defines a tuple sort with two fields, `name` (which
is a string) and `age` (which is an integer).

```
(register-tuple-sort :dog
  ((name . :string)
   (age  . :int)))
```

Values of this sort can be constructed using `(tuple-val <tuple-name> <field1> ... <fieldn>)`,
where `tuple-name` is the registered name of the tuple sort and the
fields must be provided in the same order as they appeared in the
`register-tuple-sort` form.

For example, the following statement asserts that the `:dog` value `x`
is equal to the given constant `:dog` value:
```
(z3-assert (x :dog)
           (= x (tuple-val :dog "Toto" 5)))
```

The value of a field can be accessed using `(tuple-get <tuple-name> <field-name> <val>)`,
where `tuple-name` is the registered name of the tuple sort,
`field-name` is the name of one of the fields of the tuple sort, and
`val` is a value of the tuple sort.

For example, the following statement asserts that the name of the
`:dog` value `x` is equal to `name`, and the age of `x` is greater
than 3.

```
(z3-assert (x :dog name :string)
           (and (= (tuple-get :dog name x) name)
                (> (tuple-get :dog age x) 3)))
```

## Functions

If multiple options are listed with a slash between them, then they
all refer to the same operation.

### Propositional Logic
- `=`/`==`/`equal`
- `not`
- `and`, `or`: arbitrary-arity and and or
- `implies`/`=>`
- `(distinct <v1> ... <vn>)`: true if none of `v1` through `vn` are equal to each other

### Arithmetic
Typically, functions with arity greater than unary require both
arguments to be of the same type.

- `>`,`<`,`>=`,`<=`
- `+`,`-`,`*`
- `/`: division, will result in an interpretation for division by 0 being added
- `mod`: integer modulus, will result in an interpretation for division by 0 and modulus by 0 being added
- `rem`: integer remainder, will result in an interpretation for division by 0, modulus by 0, and remainder by 0 being added
- `power`: raises the first argument to the power of the second

### Bitvector
TODO

### Sets
- `(empty-set <sort>)`: create an empty set with element sort `sort`.
- `(full-set <sort>)`: create a full set with element sort `sort`.
- `set-add`
- `set-del`
- `set-union`
- `set-intersect`
- `set-difference`
- `set-complement`
- `set-member`
- `set-subset`

### Sequences
Most of these functions operate on both strings and sequences (since
strings are essentially just a special case of sequences).

- `(seq-empty <sort>)`/`seq.empty`: create an empty sequence with element sort `sort`
- `(seq-unit <val>)`/`seq.unit`: create a length-1 sequence containing the element `val`
- `seq-concat`/`seq.++`/`str.++`
- `seq-prefix`/`seq.prefixof`/`str.prefixof`
- `seq-contains`/`seq.contains`/`str.contains`
- `str-lt`,`str-le`: lexicographic comparisons of strings
- `(seq-extract <seq> <off> <len>)`: returns the subsequence of `seq` of length `len` starting at offset `off`
- `(seq-replace <seq> <src> <dst>)`: replace the first occurrence of `src` with `dst` in `seq`
- `(seq-at <seq> <idx>)`: get the unit sequence of `seq` at index `idx`, or the empty sequence if `idx` is out of bounds
- `(seq-nth <seq> <idx>)`: get the element of `seq` at index `idx`. Under-specified if `idx` is out of bounds
- `seq-length`/`seq.len`/`str.len`
- `seq-index`/`seq.indexof`/`str.indexof`
- `(seq-last-index <seq> <substr>)`: returns the index of the last occurrence of `substr` in `seq`, or -1 if `substr` is not contained in `seq`.

### Conversions
- `(int2bv <val> <nbits>)`
- `(bv2int <val> <signed?>)`: interprets the bitvector `val` as an integer, treating as signed if `signed?` is true.
- `str-to-int`
- `int-to-str`
- `seq-to-re`

### Regular expressions
TODO

### Array
- `array-ext`: Mostly useful for internal usage, see https://github.com/Z3Prover/z3/issues/2123
