# Construcing codes for integers using primitive recursive functions

What it says on the tin. There is a module `PNNO` implementing a [parametrised natural numbers object](https://ncatlab.org/nlab/show/natural+numbers+object#withparams) and everything in the module `Functions` is defined using only the axiom that says you can define recursion with parameters: from a pair of functions `g : a -> x` and `h : a -> Nat -> x -> x` you get a unique function `J_{gh} : a -> Nat -> x` satisfying

```
J_{g,h}(p, 0) := g(p)
J_{g,h}(p, Sn) := h(p, n, J_{g,h}(p,n))
```

This is slightly cheating in that it uses idiomatic Haskell for multi-variable functions, as in `h` above, which one should perhaps more properly write as `(a, Nat, x) -> x` as I'm not wanting to assume cartesian closedness. Maybe I'll edit it to be like this later...

I can't say this is the cleanest Haskell code, definitely not efficient (given its use of recursion), but it's a sanity check on a pen-and-paper construction that [splits the idempotent](https://ncatlab.org/nlab/show/split+idempotent) `canonicalRep` on `(Nat, Nat)` as a composite `(Nat, Nat) -> Nat -> (Nat, Nat)` of primitive recursive functions, namely `retract : (Nat, Nat) -> Nat` and `include : Nat -> (Nat, Nat)`. The philosophy is that I want to rely as little as possible on manual case-definition, but also use the minimal scaffolding from Haskell to just get the recursive definition constructor.
