# Construcing codes for integers using primitive recursive functions

What it says on the tin. There is a module `PNNO` implementing a [parametrised natural numbers object](https://ncatlab.org/nlab/show/natural+numbers+object#withparams) (see also Definition 1.1 of [1]) and everything in the module `Functions` is defined using only the axiom that says you can define recursion with parameters: from a pair of functions `g : a -> b` and `h : a -> Nat -> b -> b` you get a unique function `f = J_{gh} : a -> Nat -> b` satisfying

```
f(p, 0) := g(p)
f(p, Sn) := h(p, n, f(p,n))
```

This is slightly cheating in that it uses idiomatic Haskell for multi-variable functions, as in `h` above, which one should perhaps more properly write as `(a, Nat, b) -> b` as I'm not wanting to assume cartesian closedness. But I think this is harmless.

I can't say this is the cleanest Haskell code, definitely not efficient (given its use of recursion), but it's a sanity check on a pen-and-paper construction that [splits the idempotent](https://ncatlab.org/nlab/show/split+idempotent) `canonicalRep` on `(Nat, Nat)` as a composite `(Nat, Nat) -> Nat -> (Nat, Nat)` of primitive recursive functions, namely `retract : (Nat, Nat) -> Nat` and `include : Nat -> (Nat, Nat)`. The philosophy is that I want to rely as little as possible on manual case-definition, but also use the minimal scaffolding from Haskell to just get the recursive definition constructor.

[1] Leopoldo Román, "Cartesian categories with natural numbers object"
Journal of Pure and Applied Algebra **58** issue 3 (1989) pp 267-278
<https://doi.org/10.1016/0022-4049(89)90042-X>
