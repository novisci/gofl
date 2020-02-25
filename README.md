Domain specific language for defining subgroups
-----------------------------------------------

`gofl` is a simple DSL in `R` for defining subgroups of an analytic dataset.

An example
==========

Suppose `enroll`, `enroll365`, `prev_cand`, `inc_cand`, `prev_only`, `inc_only`
are `stype::v_binary`. `age_cat` is `v_ordered` with 3 levels; `sex` is `v_nominal`
with 2 levels; and region has 4 levels.

`gofl` has 3 functions: `*`, `+`, and `z()`, correspoding to product,
disjoint union, and zoom (discard) operations. `z()` takes two arguments,
a binary or categorical variable and a set of values.
By default, `z(some_binary) == z(some_binary, TRUE)`.

```
(z(enroll)    * (z(prev_cand) + z(inc_cand)) * (age_cat*sex*region)) +
(z(enroll365) * (z(prev_only) + z(inc_only))) +
( z(age_cat, c(1, 2)) * prev_only)
```

In pseudo-code, the above would evaluate to the following set of logical expressions:

```
nrollTRUE & prev_candTRUE
enrollTRUE & prev_candTRUE & age_cat1
enrollTRUE & prev_candTRUE & age_cat2
enrollTRUE & prev_candTRUE & age_cat3
enrollTRUE & prev_candTRUE & sex1
enrollTRUE & prev_candTRUE & sex2
enrollTRUE & prev_candTRUE & region1
enrollTRUE & prev_candTRUE & region2
enrollTRUE & prev_candTRUE & region3
enrollTRUE & prev_candTRUE & region4
enrollTRUE & prev_candTRUE & age_cat1 & sex1
enrollTRUE & prev_candTRUE & age_cat2 & sex1
enrollTRUE & prev_candTRUE & age_cat3 & sex1
enrollTRUE & prev_candTRUE & age_cat1 & sex2
...
enrollTRUE & prev_candTRUE & age_cat3 & sex2 & region3
enrollTRUE & prev_candTRUE & age_cat3 & sex2 & region4
enrollTRUE & inc_candTRUE
...
enrollTRUE & inc_candTRUE & age_cat3 & sex2 & region4
enroll365TRUE & prev_onlyTRUE
enroll365TRUE & inc_onlyTRUE
(age_cat1 | age_cat2) & prev_onlyTRUE
(age_cat1 | age_cat2) & prev_onlyFALSE
```
