
## Haskell-based implementation of a stand-alone compiler for the core CFDlang domain-specific language.

This project implements a stand-alone compiler for the CFDlang DSL (domain-specific language) in Haskell.
The compiler translates tensor kernels specified in the CFDlang DSL into C functions.
This project focuses on the core elements of the CFDlang DSL to showcase the adequacy of functional programming
for parsing and for manipulating ASTs (abstract syntax trees), including semantic analysis and (intermediate) code generation.
This is of pronounced significance since relying on functional programming languages for implementations of high-level code generators is not very common in CFDlang's application domain, i.e. Computational Fluid Dynamics (CFD). 


### References

The reference implementation of the full CFDlang language appears in the repository at
[github.com/normanrink/cfdlang](https://github.com/normanrink/cfdlang).
For a comprehensive description of CFDlang and its implementation, please refer to the article
[CFDlang: High-level code generation for high-order methods in fluid dynamics](https://cfaed.tu-dresden.de/files/Images/people/chair-cc/publications/1802_Rink_RWDSL.pdf).
Note, however, that the core part of CFDlang that is accepted by the stand-alone compiler in the present repository is closer to the language described in
[Modeling of languages for tensor manipulation](https://arxiv.org/abs/1801.08771).


### Getting started

The stand-alone CFDlang compiler is built with [cabal](https://www.haskell.org/cabal/).
Running `cabal build` from the project root directory actually produces two executable binaries: 

  * `CKernelGen` translates a tensor kernel written in CFDlang into a semantically equivalent C function
    that will typically consist of sequences of loop nests and will accept multi-dimensional arrays as formal arguments.
  * `FrontEnd` implements the same functionality as `CKernelGen`, except that it prints out the CFDlang source code that the current
    invocation of `FrontEnd` operates on as well as a transformed version of this source code with _lifted tensor contractions_.
    For example, running `<build-dir>/FrontEnd <src-dir>/tests/axbxc.in` produces output
    ```
       Program with lifted periods:
       var in a : [2 3]
       var in b : [2 3]
       var in c : [2 3]
       var in u : [3 3 3]
       var out w : [2 2 2]

       _t0 = (((a#b)#c)#u).[1 6]
       _t1 = _t0.[2 5]
       w = _t1.[3 4]
    ```
    followed by C source code that implements this CFDlang kernel.
    Tensor contractions are lifted in the CFDlang kernel above because every expression in an assignment contains at most one contraction (denoted by `.[m n]`) at the top level;
    no nested contractions appear inside expressions.
  
Both the `CKernelGen` and `FrontEnd` binaries accept a command line argument that should be the path to a CFDlang source file.
Alternatively, CFDlang source code can be entered on the console after either of the `CKernelGen` or `FrontEnd` executables has been invoked.
