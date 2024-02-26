# ATL - A Typed Language

What if a language doesn’t have types? In the old vanilla JavaScript, objects can be defined as literals, and constructors are just glorified functions. Obviously this is not a new idea and besides, it’s a horrible idea: programmers have to eyeball the type of every variable to prevent errors happening at runtime. 

But what if we make a type system that formalizes this idea, so that it’s less “dynamic”? First of all, this language shouldn’t need, or even prevents programmers from type declaration. But how could this work? 

- If a type is declared but never accessed anywhere, it is redundant code to the program

- If a type is referred to at some point but there is nothing to be done with its members, then this type is useless anyway

- If in the most common case, a member is instantiated from the type and it is assigned, referenced or passed around as an argument, then intuitively we know it must have these properties to make the program work.

	For example:

	```
	SomeType x = ...;
	x.field = 1;
	someFunc(OtherType t) { ... }
	someFunc(x);
	```
	
	Then we know:
	- `x` has a key named `field` and probably is of type `int`
	- `x` must be a subtype of `OtherType`

Now even without type declaration, we know there must exist a type that is inhabited by a member called `x` and have the aforementioned properties.

---

Another Example (syntax is modified for demonstration):

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

The variable `tree` has type `Tree = {'Leaf} ∪ Branch` where `Branch =  {'Branch} × Tree × Tree`

## Singleton Type

As you can see, types in this type system is very different from a typical one. Specifically, to make the language concise and useful, it must introduce the Singleton Type, which is constructed from an expression. If we consider a type to be a special set, then the Singleton Type is inhabited by only one member, denoted as `{x}` where `x` is the said member.

The quirk about this type is that it is dependent on `x`, and `x` can be any expression that has a type. For example:

- `{1}` is a type containing only `1`. It is a subtype of `int`

- `{a}` where is a singleton type of unknown variable `a`. What it means is that, suppose (syntax is modified for demonstration):

  ```
  x = a; // x : {a}
  x = 2; // type error, there is no guarantee that a == 2
  ```

How can we intentionally break this type system? Let’s say 

```
a = 2 // a : A = [1, 2]
x = a; // x : X = {a}
a = 1;
```

But `x` still has value of 2. `x : {a}` implies that the only value that `x` can take is `a`, thus `x == a` is always true, which raises a contradiction.

To resolve this, we need to introduce the concept of immutable types.

## Immutable Type

To avoid the conflict mentioned above, we restrict the singleton types to accept only immutable types. 

The syntax is:

```
var! x = ...; // x is the type of anything that is immutable
int! x = ...; // x is the type of immutable int
```

How do we break it?

```
x = {a: 1, b: 2} // x : {a: int, b: int}
y = x // 
```







It's worth mentioning that, the type `Tree` is inferred by the type checker, rather than defined.
In ATL, programs can't define types; but rather types are inferred by the values that inhabitates.

Since the type `Sum` doesn't contain any quantifiers, it is regarded as a fact. Thus, if the
following code tries to pass in an incompatible type, it will complain:

```
var x; // x : ⊤
sum(x); // not ok, ⊤ is not a subtype of Tree
```

