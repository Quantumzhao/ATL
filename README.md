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

Another Example:

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



Var


$$
\def\glb{\mathrm{glb}}
\def\bool{\mathrm{bool}}
\begin{prooftree}
\AxiomC{}
\UnaryInfC{$A\cup\{x\mapsto\tau\},\ C\vdash x:\tau$}
\end{prooftree}
$$

Indexing

$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash x:\tau=\langle\tau,\ n,\ \ldots\tau_i\ldots\rangle,\ i:\tau_j$}
\UnaryInfC{$A,\ C\cup\overline{C_i}\vdash x[i]:\tau_i,\quad i:\tau_j\cap[0,n)$}
\end{prooftree}
$$

Projection

$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash x:\tau\cup\{x.y\mapsto\tau'\}$}
\UnaryInfC{$A,\ C\vdash x.y:\tau'$}
\end{prooftree}
%\quad
%\begin{prooftree}
%\AxiomC{$A,\ C\vdash x:\tau$}
%\UnaryInfC{$A,\ C\vdash x.y:\top,\quad x:\tau\cap\{x.y\mapsto\tau'\}$}
%\end{prooftree}
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
- $\overline{\tau_i}$ are all the $\tau_i$ that are proper subtypes of $\tau$

Array 2

$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash e:\tau_e$}
\UnaryInfC{$A,\ C\vdash \tau[e]:\langle \tau,\ n,\ \empty\rangle,\quad e:\tau_e'=\glb(\tau_e,\Z),\quad n=\max\tau_e'$}
\end{prooftree}
$$

---

Assign

$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash e:\tau$}
\AxiomC{$A\cup\{x\mapsto\tau\},C\vdash x:\tau$}
\BinaryInfC{$A,\ (C-\overline{C_x})\cup C[e\mapsto x]\vdash x=e:\circ,\quad x:\tau$}
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
\AxiomC{$A,\ C\vdash e_b:\bool,\quad$}
\UnaryInfC{$A,\ C\vdash \mathtt{if}(e_b)\ e_t\ \mathtt{else}\ e_f:\circ$}
\end{prooftree}
$$

- $e_b$ introduces constraints between terms
- terms which types are updated in $e_t$ and $e_f$ receive a new union type after the `if` statement

While

- same as `if`
- in addition, all terms that receive updated types must has a subtype of what they begin with

Apply

$$
\begin{prooftree}
\AxiomC{$A,\ C\vdash f:\tau_f,e_1:\tau_1,\ldots,e_n:\tau_n$}
\UnaryInfC{$A,\ C-\overline{C_x}\vdash f(e_1,\ldots,e_n):\beta,\quad\tau_f=\alpha_1,\ldots,\alpha_n\rightarrow\alpha,\quad\beta\subseteq\alpha,\quad\tau_1\subseteq\alpha_1,\ldots,\tau_n\subseteq\alpha_n$}
\end{prooftree}
$$

- $\overline{C_x}$ are all the constraints on $x$ which values are changed

Pattern Matching
$$
\begin{prooftree}
\AxiomC{$
\begin{gather}
A\cup\{v\mapsto\tau_v|v\in Var(p_1)\},\ C\vdash p_1:\alpha_1,\ b_1:\beta_1\\
\vdots\\
A,\ C\vdash e:\tau,\ C_e\quad A\cup\{v\mapsto\tau_v|v\in Var(p_n)\}\vdash p_n:\alpha_n,\ b_n:\beta_n
\end{gather}$}
\UnaryInfC{$A,\ C\vdash(\mathtt{switch}\ (e)\ p_1\rightarrow b_1;\ldots;p_n\rightarrow b_n):\circ$}
\end{prooftree}
$$
