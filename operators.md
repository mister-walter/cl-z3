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

Names in parentheses after a function call denote alternative names
for the same function. Square brackets around a parameter name
indicate that the parameter can only be a constant value (it cannot be
a Z3 variable or symbolic value, for example). Angle brackets around a
parameter name indicate that the parameter may have a symbolic value.

### Propositional Logic
- `(equal <x> <y>)` (`=`,`==`): true if `x` and `y` are equal under Z3's notion of equality
- `(not <x>)`: Boolean negation
- `(and <v1> ... <vn>)`: Boolean conjunction of `v1` through `vn`
- `(or <v1> ... <vn>)`: Boolean disjunction of `v1` through `vn`
- `(implies <v1> <v2>)` (`=>`): Boolean implication
- `(distinct <v1> ... <vn>)`: true if none of `v1` through `vn` are equal to each other
- `(atleast <v1> ... <vn> <k>)` is true if `k` or more of `v1` through `vn` are true
- `(atmost <v1> ... <vn> <k>)` is true if `k` or fewer of `v1` through `vn` are true

### Arithmetic
Typically, functions with arity greater than unary require both
arguments to be of the same sort. This is not an issue at the moment,
as the interface currently does not support any numeric sorts aside
from integers.

Note that operations that may cause exceptions in other languages
(like division by zero) are underspecified in Z3. This means that Z3
treats `(/ x 0)` as an uninterpreted function that it may assign any
interpretation to. This can lead to unexpected behavior if you're not
careful.

For example, Z3 reports that the following is satisfiable, since it
can assign `x` and `y` different values, and has the flexibility to
have division by 0 for the value of `x` return 3, and division by 0
for the value of `y` return 4.

```
(z3-assert (x :int y :int)
           (and (= (/ x 0) 3)
                (= (/ y 0) 4)))
(check-sat)
```

- `(> <x> <y>)`,`(< <x> <y>)`,`(>= <x> <y>)`,`(<= <x> <y>)`: Comparisons
  between two numbers
- `(+ <v1> ... <vn>)`: Numerical (real) addition of all `vi`
- `(- <x>)`: Unary negation of the number `x`
- `(- <v1> ... <vn>)`: Numerical (real) subtraction of `v2` through `vn`
  from `v1`. More than one argument must be provided.
- `(* <v1> ... <vn>)`: Numerical (real) multiplication of all `vi`
- `(/ <x> <y>)`: division, will result in an interpretation for division
  by 0 being added
- `(mod <x> <y>)`: integer modulus, will result in an interpretation for
  division by 0 and modulus by 0 being added
- `(rem <x> <y>)`: integer remainder, will result in an interpretation
  for division by 0, modulus by 0, and remainder by 0 being added
- `(power <x> <y>)`: raises `x` to the power of `y`

### Function Application
- `(_ <fn> <arg1> ... <argn>)`: apply the function `fn` to the arguments
  `arg1` through `argn`

### Quantifiers
- `(exists (<v1> <v1sort> ... <vn> <vnsort>) <body>)`: true if there
  exist assignments for `v1`, ... `vn` of sort `v1sort`, ... ,
  `vnsort` respectively such that the body is true under the
  assignments

- `(forall (<v1> <v1sort> ... <vn> <vnsort>) <body>)`: true if for
  all assignments for `v1`, ... `vn` of sort `v1sort`, ... , `vnsort`
  respectively, the body is true under the assignments

### Bitvectors
TODO

### Sequences
Most of these functions operate on both strings and sequences (since
strings are essentially just a special case of sequences).

- `(seq-empty <sort>)`/`seq.empty`: create an empty sequence with
  element sort `sort`
- `(seq-unit <val>)`/`seq.unit`: create a length-1 sequence containing
  the element `val`
- `(seq-concat <seq1> <seq2>)` (`seq.++`,`str.++`): concatenate `seq1`
  and `seq2`
- `(seq-prefix <seqp> <seq>)` (`seq.prefixof`,`str.prefixof`): true if
  `seqp` is a prefix of `seq`
- `(seq-contains <container> <containee>)` (`seq.contains`/`str.contains`):
  true if `container` contains the sequence `containee`
- `(str-lt <x> <y>)`,`(str-le <x> <y>)`: lexicographic comparisons of
  strings
- `(seq-extract <seq> <off> <len>)`: returns the subsequence of `seq`
  of length `len` starting at offset `off`
- `(seq-replace <seq> <src> <dst>)`: replace the first occurrence of
  `src` with `dst` in `seq`
- `(seq-at <seq> <idx>)`: get the unit sequence of `seq` at index `idx`,
  or the empty sequence if `idx` is out of bounds
- `(seq-nth <seq> <idx>)`: get the element of `seq` at index `idx`.
  Under-specified if `idx` is out of bounds
- `(seq-length <seq>)` (`seq.len`,`str.len`): get the length of `seq`
- `(seq-index <seq> <subseq> <off>)` (`seq.indexof`,`str.indexof`):
  returns the index of the first occurrence of the sequence `subseq`
  in `seq` starting from offset `off`, or -1 if `subseq` does not
  occur in `seq` after offset `off`
- `(seq-last-index <seq> <substr>)`: returns the index of the last
  occurrence of the sequence `substr` in `seq`, or -1 if `substr` is
  not contained in `seq`

### Conversions
- `(int2bv [nbits] <val>)`: Interprets the integer `val` as a bitvector
  of length `nbits`
- `(bv2int <val> [signed?])`: Interprets the bitvector `val` as an
  integer, treating as signed if `signed?` is true.
- `(str-to-int <x>)`: Converts string `x` into an integer. Returns -1 if
  `x` cannot be converted into an integer.
- `(int-to-str <x>)`: Converts integer `x` into a string.
- `seq-to-re`

### Regular expressions
TODO

### Sets
Note that the translation of set values reported by Z3 is not
currently supported.

- `(empty-set <sort>)`: create an empty set with element sort `sort`.
- `(full-set <sort>)`: create a full set with element sort `sort`.
- `(set-add <set> <elt>)`
- `(set-del <set> <elt>)`
- `(set-union <set1> <set2>)`
- `(set-intersect <set1> <set2>)`
- `(set-difference <set1> <set2)`
- `(set-complement <set>)`
- `(set-member <elt> <set>)`
- `(set-subset <set1> <set2>)`

### Arrays
TODO
- `array-ext`: Mostly useful for internal usage, see https://github.com/Z3Prover/z3/issues/2123
