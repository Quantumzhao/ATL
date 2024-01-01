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
tree : Tree = Leaf ∪ Branch
'Leaf : Leaf = {'Leaf} <: String
0 : `0 = {0} <: Int
'Branch : Branch = {'Branch} <: String
l : L = Tree
r : R = Tree
+ : `+ = Int, Int -> Int
sum : Sum = Tree -> Int
```

It's worth mentioning that, the type `Tree` is inferred by the type checker, rather than defined.
In ATL, programs can't define types; but rather types are inferred by the values that inhabitates.
