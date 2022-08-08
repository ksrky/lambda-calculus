# Lambda calculus

Haskell implementation of Types and Programming Languages written by Benjamin C. Pierce

## [untyped](https://github.com/ksrky/lambda-calculus/tree/master/src/untyped)

Untyped lambda calculus

### Syntax

```
t ::=                       :terms
      x                     variable
      \x.t                  abstraction
      t t                   application
```

### Reference

- TaPL chap7

## [typed](https://github.com/ksrky/lambda-calculus/tree/master/src/typed)

$\lambda_{\rightarrow}$: Simply typed lambda calculus / Propositional logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:T.t                abstraction
      t t                   application
      true                  constant true
      false                 constant false

T ::=                       :types
      Bool                  type of booleans
      T->T                  type of functions
```

### Reference

- TaPL chap10

## [systemf](https://github.com/ksrky/lambda-calculus/tree/master/src/systemf)

$\lambda 2$: System F (`typed`+ parametric polymophism) / Second-order propositional logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:T.t                abstraction
      t t                   application
      \X.t                  type abstraction
      t [T]                 type application

T ::=                       :types
      X                     type variable
      T->T                  type of functions
      forall X.T            universal type
```

### Reference

- TaPL chap23, 25

## [fomega](https://github.com/ksrky/lambda-calculus/tree/master/src/fomega)

$\lambda \omega$: System $\mathrm{F_{\omega}}$ (`systemf`+ type operators) / Higher-order propositional logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:T.t                abstraction
      t t                   application
      \X:K.t                type abstraction
      t [T]                 type application

T ::=                       :types
      X                     type variable
      T->T                  type of functions
      forall X:K.T          universal type
      \X:K.T                operator abstraction
      T T                   operator application

K ::=                       :kinds
      *                     kind of proper types
      K->K                  kind of operators
```

### Reference

- TaPL chap29, 30

## [lambdapi](https://github.com/ksrky/lambda-calculus/tree/master/src/lambdapi)

$\lambda \Pi$: Lambda Pi (`typed`+ dependent type) / First-order predicate logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:t.t                abstraction
      t t                   application
      *                     type of types
      forall x:t.t        　dependent product type
```

note: Quantification of a variable is limited to the term level.

### Reference

- Andres Löh, Conor McBride, and Wouter Swierstra. 2010. A Tutorial Implementation of a Dependently Typed Lambda Calculus. Fundam. Inf. 102, 2 (April 2010), 177–207.

## lambdapi2

$\lambda \Pi 2$: Lambda Pi2 / Second-order predicate logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:t.t                abstraction
      t t                   application
      *                     type of types
      forall x:t.t          dependent product type
```

note: Quantification of a variable is limited to the type level.

not yet

## coc

$\lambda \mathrm{C}$: Calculus of Construction / Higher-order predicate logic

### Syntax

```
t ::=                       :terms
      x                     variable
      \x:t.t                abstraction
      t t                   application
      *                     type of types
      forall x:t.t          dependent product type
```

not yet
