# Monadic Logic Decider

This program decides propositions in *monadic predicate logic* -- a subset of first-order logic -- by converting monadic schemata into boolean propositions, which are then decided via truth tables. I discovered the (very inefficient) conversion algorithm when taking an introductory logic course. But at least this shows that monadic predicate logic is decidable.

## What exactly is monadic predicate logic?

Taken from [Wikipedia](https://en.wikipedia.org/wiki/Monadic_predicate_calculus):
> In logic, the **monadic predicate calculus** (also called **monadic first-order logic**) is the fragment of first-order logic in which all relation symbols in the signature are monadic (that is, they take only one argument), and there are no function symbols. All atomic formulas are thus of the form $P(x)$, where $P$ is a relation symbol and $x$ is a variable.
> 
> Monadic predicate calculus can be contrasted with **polyadic predicate calculus**, which allows relation symbols that take two or more arguments.