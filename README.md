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

Var


$$
\def\glb{\mathrm{glb}}
\begin{prooftree}
\AxiomC{}
\UnaryInfC{$A\cup\{x\mapsto\tau\},\ C\vdash x:\tau$}
\end{prooftree}
$$
Indexed Var
$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash x:\tau=\langle\tau,\ n,\ \ldots\tau_i\ldots\rangle,\ i:\tau_i\subseteq\Z$}
\UnaryInfC{$A,\ C\cup\overline{C_i}\vdash x[i]:\tau_i,\quad i:\tau_i\cap[0,n)$}
\end{prooftree}
$$
Projected Var
$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash x:\tau\cup\{x.y\mapsto\tau'\}$}
\UnaryInfC{$A,\ C\cup\overline{C_y}\vdash x.y:\tau'$}
\end{prooftree}
\quad
\begin{prooftree}
\AxiomC{$A,\ C\vdash x:\tau$}
\UnaryInfC{$A,\ C\vdash x.y:\top,\quad x:\tau\cap\{x.y\mapsto\tau'\}$}
\end{prooftree}
$$

---

Struct
$$
\begin{prooftree}
\AxiomC{$A,C \vdash e_1:\tau_1,\ \ldots,\ e_n:\tau_n$}
\UnaryInfC{$A,C \vdash c(e_1,\ \ldots,\ e_n):c(\tau_1,\ \ldots,\ \tau_n)$}
\end{prooftree}
$$
---

Array 1
$$
\def\lub{\mathrm{lub}}
\begin{prooftree}
\AxiomC{$A,\ C\vdash e_1:\tau_1,\ \ldots,\ e_n:\tau_n$}
\UnaryInfC{$A,\ C\cup\overline{C_{i}}\vdash [e_1,\ \ldots,\ e_n]:\langle \tau,\ n,\ \overline{\tau_i}\rangle$}
\end{prooftree}
$$
where

- $\overline{C_{i}}$ are all the constraints that are related to $e_i$, but substitute $e_i$ for indexed variables. If $e_i$ is not a normal form value, nothing happens
- $\tau:\underset{i}\lub(\tau_i)$​
- $\overline{\tau_i}$ are all the $\tau_i$ that are proper subtypes of $\tau$​

Array 2
$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash e:\tau_e$}
\UnaryInfC{$A,\ C\vdash \tau[e]:\langle \tau,\ n,\ \empty\rangle,\quad e:\tau_e'=\glb(\tau_e,\Z),\quad n=\max\tau_e'$}
\end{prooftree}
$$

---

Declare
$$
\begin{prooftree}
\AxiomC{$A,C \vdash x:\tau$}
\UnaryInfC{$A,C \vdash \tau\ x:\circ$}
\end{prooftree}
$$
---

Assign
$$
\begin{prooftree}
\AxiomC{$A,C\cup \overline{C_x}\vdash x:\tau_x,\ e:\tau_e$}
\UnaryInfC{$A,\ C\vdash x:=e:\circ,\quad x,e:\glb(\tau_x,\tau_e)$}
\end{prooftree}
$$
---

Abstraction
$$
\begin{prooftree}
\AxiomC{$A\cup\{x_1\mapsto\tau_1,\ldots,x_n\mapsto\tau_n,f\mapsto\tau_f\},\ C\vdash e:\tau_e$}
\UnaryInfC{$A,\ C\vdash\tau_f\ f(x_1,\ldots x_n)\rightarrow e:\circ,\quad\tau_f=((\tau_1,\ldots,\tau_n)\rightarrow\tau_e)$}
\end{prooftree}
$$
---

If
$$
\begin{prooftree}
\AxiomC{$$}
\UnaryInfC{$A,\ C\vdash \mathtt{if}(e_b)\ e_t\ \mathtt{else}\ e_f:\circ$}
\end{prooftree}
$$


While

Apply
$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash f:\tau_f,e_1:\tau_1,\ldots,e_n:\tau_n$}
\UnaryInfC{$A,\ C\vdash f(e_1,\ldots,e_n):\beta,\quad\tau_f=\alpha_1,\ldots,\alpha_n\rightarrow\alpha,\quad\beta\subseteq\alpha,\quad\tau_1\subseteq\alpha_1,\ldots,\tau_n\subseteq\alpha_n$}
\end{prooftree}
$$
Pattern Matching
$$
\begin{prooftree}
\AxiomC{$$}
\UnaryInfC{$A,\ C\vdash(\mathtt{switch}\ (e)\ p_1\rightarrow b_1;\ldots;p_n\rightarrow b_n):\circ$}
\end{prooftree}
$$
