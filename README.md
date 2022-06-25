# ash

A tree walking interpreter for a subset of [R5RS](https://schemers.org/Documents/Standards/R5RS/)
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)).

## Details

```
         ╭─────────╮                      ╭────────────╮                ╭────────╮
Text ──► │  Parse  │ ─── Scheme value ──► │  AstParse  │ ──── AST ────► │  Eval  │ ───► Effects and result
         ╰─────────╯                      ╰────────────╯                ╰────────╯
```

Source code is parsed into [S-expressions](https://en.wikipedia.org/wiki/S-expression), represented as Scheme data.  
This is then parsed into an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST), which
contains primitive Scheme expressions.  
The AST is then evaluated.

The primary purpose of ash has been learning and fun. There has been no attempt to improve performance.  
ash was initially based on [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
and [Write You A Scheme, Version 2.0](https://wespiser.com/writings/wyas/00_overview.html), but currently has little in common with
these tutorials.

## Differences from R5RS

The following are not supported

* Mutation
* Macros
* Continuations
* Non-integral numbers
* Quasiquotations

Almost all standard procedures, excluding those involving the above features, are available.

## Installing

GHC and Cabal are required. The recommended way to install these is using GHCup.  
See the downloads page on [haskell.org](https://www.haskell.org/downloads/) for details.

```
git clone https://github.com/meooow25/ash.git
cd ash
cabal install
```

Then simply run
```
ash -h
```

## Future features, maybe

* Macros
* Flonums
* Compile to byte code
