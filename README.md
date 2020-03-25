Domain specific language for defining subgroups
-----------------------------------------------

`gofl` is a simple DSL in `R` for defining subgroups of an analytic dataset. The syntax is similar to `R`'s native model formula syntax. 

A `gofl` formula has 5 operators: `+`, `:`, `*`, `.zoom()` (or `z)()`), and `tag()`. The first three operate similar to how they work in a model formula. For example, suppose the variables `A` and `B` have each have 2 levels. Under the hood, `gofl` is creating a matrix with one row per group.

The sum operation `+` results in one group for each level of each variable:

```
A1 A2 B1 B2 
1  0  0  0
0  1  0  0
0  0  1  0
0  0  0  1
```

The product operator `:` yields in one group the cartesian product of `A` and `B`:

```
A1 A2 B1 B2 
1  0  1  0
1  0  0  1
0  1  1  0
0  1  0  1
```

The operator `*` is equivalent to `A + B + A:B`:

```
A1 A2 B1 B2 
1  0  0  0
0  1  0  0
0  0  1  0
0  0  0  1
1  0  1  0
1  0  0  1
0  1  1  0
0  1  0  1
```

The `.zoom` function can be used to filter to levels of interest; e.g. `.zoom(A, levels = 1) + B` corresponds to the matrix:

```
A1 A2 B1 B2 
1  0  0  0
0  0  1  0
0  0  0  1
```

The `tag` function is used to tag particular groups. See the package vignette for example.
