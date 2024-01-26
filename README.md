# ATL - A Typed Language

What you can do in ATL:

```
sum(tree) {
    switch (tree) {
        case 'Leaf: // the ' notation is a shorthand for string, 'Leaf == "Leaf"
            return 1;
        case ('Branch, l, r):
            return sum(l) + sum(r);
    }
}
```

Looks trivial for a typical dynamic language, but the plot twist is that ATL is statically typed.
In fact it fully type checks:

```
tree : Tree = {'Leaf} ∪ Branch
'Leaf : {'Leaf} <: String
1 : {1} <: Int
'Branch : {'Branch} <: String
l : L = Tree
r : R = Tree
('Branch, l, r) : {'Branch} × Tree × Tree
+ : `+ = Int, Int -> Int
sum(l) + sum(r) : Int
sum : Sum = Tree -> Int
```

It's worth mentioning that, the type `Tree` is inferred by the type checker, rather than defined.
In ATL, programs can't define types; but rather types are inferred by the values that inhabitates.

Since the type `Sum` doesn't contain any quantifiers, it is regarded as a fact. Thus, if the
following code tries to pass in an incompatible type, it will complain:

```
var x; // x : ⊤
sum(x); // not ok, ⊤ is not a subtype of Tree
```

<!-- How is it different from TypeScript?

Example:
```
mod2(n) {
    if (n < 2) {
        return n;
    } else {
        return mod2(n - 2);
    }
}

var x;
var a = 8;
if (mod2(a) == 0) {
    x = 1;
} else {
    x = 3;
}
var y = x + a;
```

What would be the type of `y`?

The type checker roughly follows:
1. We observe the relation `mod2(n)`: it is a recursive function with one base case and one 
inductive hypothesis
2. Combined with the guard `n < 2`, thus `n <: {0, 1}` and the return type can only be a subtype of `{0, 1}`
3. In `mod2(a) == 0`, `{0} <: {0, 1}`, there exists an equality relation, so `mod2(a) == 0 : Bool` type checks
4. In the lower `if` statement, we want to study the property of `mod2(n) == 0` in general
    1. Either `n == 0` or `mod2(n - 2) = 0`, for all `n` such that `mod2(n) == 0`
    2. Which is equivalent to, `mod2(n) == 0 => mod2(n - 2) == 0`. 
    3. Consider the class where, for all `n` such that `mod2(n) == 0`, then there must exist `n'` such that 
    `n' - 2 == 0`, *assuming the program terminates*. Similarly, there will be `n'' - 2 == n'`, etc.
    4. So the class of `mod2(n) == 0` is `{0, 2, 4, ...}` generated by the only one base case and 
  inverse of IH, namely `n' = n + 2`. Because of the assumtion on determinism, program termination, the existence of inverse and
  that the inverse is well-defined, this inductive definition of the class of `mod2(n) == 0` can exist.
    5. If the base case t₀ is a subtype of `Int`, and IH is a map from any t₁ ∈ T to t₂ ∈ T,
then T ∈ `Int`. 
    4. (5) is true, proved by the existence of `n = 0` and the map `f(n) = n + 2`
    5. Thus `a ∈ {0, 2, 4, ...} => mod2(a) == 0`
    6. `{8} <: {0, 2, 4, ...}`, thus `mod2(a) == 0` is always true
    7. Similarly, the opposite is always false
5. Now we focus on the `if` statement. 
  The program flow can only take the `if_true` path, thus the program is equivalent to 
  `var x; x = 1;`
6. The type for x in this case is `{1}`
7. The only inhabitants of `{1}` and `{8}` are `1` and `8`, so the range of `+` is `{1+8} = {9}`
8. `y : {9}` -->