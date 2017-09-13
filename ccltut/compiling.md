# Writing an LLVM compiler using continuation passing

Continuation passing compilers have a strong history and [Appel's
book](http://dl.acm.org/citation.cfm?id=1512932) is a must read for anyone
interested in the area.

Despite the large body of existing work on CPS compilers, I wanted to give an
introduction to writing a simple `compile` function on first order expressions
using Idris.  My aim is to give just enough information about the subject to
support future posts---we won't bother to make much use of the flexibility
compiling with continuations grants.

This post is written as literate Idris ([grab the code
here](https://github.com/christopherschwaab/blog/ccltut/)) and passes totality
checking.  Syntax highlighted pieces of code are the extractable bits;
unhighlighted code is merely illustrative and probably won't compile.

To extract the code, try

    $ sed -n '/~~~~{.hask/,/~~~~/p' compiling.md |grep -v '~~~~' > compiling.idr

or for the final LLVM program

    $ sed -n '/~~~~{.llvm/,/~~~~/p' compiling.md |grep -v '~~~~' > a.ll

## Compiling a language of addition
Consider a simple language of addition on integers.  The language obviously
needs an `Add` term which should take two add-language-sub-expressions; the
language will also need support for integer literals/immediates and variables.

~~~~{.haskell}
namespace Exp
  data AddExp : Type -> Type where
    Var : var -> AddExp var
    Lit : Int -> AddExp var
    Add : AddExp var -> AddExp var -> AddExp var
~~~~

The goal is to compile add expressions to LLVM assembly.  Intuitively this
machine will have three instructions (add/print/halt) which all operate on
values (i.e. don't accept complicated expressions as arguments).  A value in
this case is either an integer literal/immediate (e.g. 2, 3, etc.) or a
register (e.g. %x0).  The type of values is readily expressed in Idris

~~~~{.haskell}
data Value : (var : Type) -> Type where
  Var : var -> Value var
  Lit : Int -> Value var
~~~~

We take registers to be of abstract type `var` allowing a choice of
representation such as strings: e.g.  "%tmp0", "%tmp1", etc.; a counter: 1, 2,
3 which map to %1, %2, %3, etc.; or any other preferred representation such as
the use of real Idris variables.

Cobbling together the type of instructions in Idris requires just three
constructors:

  1. `Add`: add two source values and produce a result in a destination register;
  2. `Print`: print any value; and
  3. `Halt`: exit the program.

Although these are broadly sufficient, let's start with an obvious but
problematic encoding of instructions to motivate our ultimate solution

~~~~{.haskell}
data LlvmInstr : Type -> Type where
  Print1 : Value var -> LlvmInstr var
  Halt1 : LlvmInstr var
  Add1 : var -> Value var -> Value var -> LlvmInstr var
~~~~

Of course a program is a sequence of instructions so as a necessary extension
of `LlvmInstr` we should consider the type of programs `LlvmProgram`.
Naturally we model programs as lists of instructions which are to be executed
in order.  This intuitively captures the flat, linear way that assembly
programs are written down:

    LlvmProgram : Type -> Type
    LlvmProgram var = List (LlvmInstr var)

Unfortunately however, the naive approach to compiling addition expressions to
a list of llvm instructions will fail.  To understand why, consider the difficulty
of compiling naked values directly to an assembly instruction

    compile1 : AddExp String -> LlvmProgram String
    compile1 (Var x) = ?compileVar
    compile1 (Lit z) = ?compileLit
    compile1 (Add e1 e2) = compile e1
                        ++ compile e2
                        ++ ?addE1E2 {- use results of compile e1/e2 in Add1 instr? -}

There are two obviously identifiable problems with the above

  1. as noted it's unclear how to compile variables and literals; and 
  2. extracting the results of `e1` and `e2` to add together is unobvious
     (additionally, we should ask where this result could be placed).

Variables and int values don't have a natural representation in assembly
because values aren't valid instructions; contrast this with the expression
language: values _are_ valid expressions.  A naked value in an expression is
simply returned, e.g. the expression "2" simply evaluates to the result "2";
however even if we had some return instruction `Ret`, the compiled program
`compile (Lit z) = [Ret z]` will be wrong.  Compiling values to return
statements means programs such as `Add (Lit 2) (Lit 3)` produce

    compile1 (Add (Lit 2) (Lit 3))
       = compile1 (Lit 2)
      ++ compile1 (Lit 3)
       ...
       = [Ret 2]
      ++ [Ret 3]
       ...
       = [Ret 2, Ret 3, ...]

Which immediately returns without performing any addition!  What about our second
problem: where do the intermediate results of `compile e1` and `compile e2` go?
Where is addition of those two results placed?

A simple solution to both of the above problems is for the compile function to
produce not only an `LlvmProgram`, but additionally a value or register which
is where the result of the list of instructions was placed.  You probably saw
this coming if you've written a toy compiler with an explicit flattening stage
taking e.g. _(x + y) + 2_ to _tmp1 = x + y_ and _tmp2 = tmp1 + 2_.  In the case
of flattening, _tmp1_ would need to be bubbled up from the flattening of
sub-tree _x+y_ to be used when processing the right sub-tree _2_.  In Idris a
naive attempt at this will fail because we need to generate variable names
(like _tmp1_, _tmp2_) and have no means of doing so

    compile2 : AddExp String -> (String, LlvmProgram)
    compile2 (Var x) = (x, [])
    compile2 (Lit z) = (?litResult, [])
    compile2 (Add e1 e2) = let (r1, is1) = compile e1 in
                           let (r2, is2) = compile e2 in
                           is1 ++ is2 ++ [Add1 ?res (Var r1) (Var r2)]

Annoyingly this generation of fresh names (_tmp1_, _tmp2_) for intermediate
results requires state.  Even a simple counter will fail unless the counter's
value is carefully threaded up and down the recursion tere. If this isn't
apparent consider that we can't know a priori how many values will be required
by a given sub-tree  so we can't properly update the counter when we recurse
into the next sub-tree (e.g. what is the value of the counter after recursing
into the left sub-tree of unknown size?).  The standard functional solution to
state threading would introduce a triplet of `(Int, String, LlvmProgram)`
instead of the current pair `(String, LlvmProgram)`.  Of course using a state
monad might improve the situation but under the hood that's just continuation
based!

The use of continuations begs the question: what do continuations provide other
than state threading?

Continuations provide the introduction of fresh variable names (name binding,
`\tmp1 => ...`) which is precisely the problem we're trying to solve.  Instead
of e.g.  generating some string name "tmp1" we can ask the compiler for a name
by using a lambda `\tmp1 => func tmp1`.  Of course this name can never escape
above its scope so we can only push information _down_, not bubble it up (with
e.g.  a pair `(tmp1, [])`). This should inform our strategy: don't use explicit
intermediate names such as a concrete destination register in the `Add1`
instruction.  Instead the instruction-list structure of a program should be
folded into the `LlvmInstr` type directly, allowing for intermediate results to
be directly passed as a bound variable to the remaining
instruction-list/program.

Let's replace the `LlvmInstr` type with a type of programs: `Llvm`.  The
intermediate name used by the `Add1` instruction will be replaced with a
continuation from the resulting sum, to the program's tail.

~~~~{.haskell}
data Llvm : Type -> Type where
  Add : (v : Value var) -> (w : Value var) -> (program : (z : var) -> Llvm var) -> Llvm var
  Print : Value var -> Llvm var -> Llvm var
  Halt : Llvm var
~~~~

The `Print` and `Halt` statements haven't changed much.  Rather than a program
being a list of a instructions we directly embed the list structure into the
`Llvm` type just as promised e.g. `Print` takes a value to print along with the
rest of the program (effectively a list tail) `prog : Llvm var`.  The `Add`
instruction has a continuation from its intermediate result `z` to the program
tail.

Although `Add` is slightly strange, it's intuitively read as: add takes two
values _v_, and _w_, and produces a result _z = v+w_ which is used in the
proceeding list of instructions i.e. the rest of the program.

Lets consider a simple transformation from explicit destination registers to
the continuation encoding to clarify the technique

      Add1 "%tmp1" (Lit 2) (Lit 3) ::
      Add1 "%tmp2" (Var "%tmp1") (Lit 4) ::
      Add1 "%tmp3" (Var "%tmp1") (Var "%tmp2") ::
      []
    =>
      Add (Lit 2) (Lit 3) (\tmp1 =>
      Add (Var tmp1) (Lit 4) (\tmp2 =>
      Add (Var tmp1) (Var tmp2) (\tmp3
      Halt)))

Writing `compile` is trivial using continuations.  At each step of compilation,
the expression being compiled produces some intermediate result value _tmp_
which is fed into the compilation of the rest of the program

~~~~{.haskell}
compile : AddExp var -> ((tmp : Value var) -> Llvm var) -> Llvm var
compile (Var x) compileProg = compileProg (Var x)
compile (Lit z) compileProg = compileProg (Lit z)
compile (Add e1 e2) compileProg =
  compile e1 (\tmp1 =>
  compile e2 (\tmp2 =>
  Add tmp1 tmp2 (\result =>
  compileProg (Var result))))
~~~~

Of course at the end of the day we're producing a string/file to feed into llvm
so the need to generate names can't ultimately be escaped. Fortunately since
we've eliminated the tree structure and are left with a linear stream of
instructions we no longer need state to track used names.  In the case of LLVM
it's particularly easy because a counter can be used to directly generate names
such as "%1".  Let's make a pretty print function so we can compile and run our
programs

~~~~{.haskell}
llvmValue : Value String -> String
llvmValue (Var x) = x
llvmValue (Lit z) = show z

prettyLlvm : Int -> Llvm String -> String
prettyLlvm name (Add v w prog) =
  let result = "%" ++ show name
  in result ++ " = add nsw i32 " ++ llvmValue v ++ ", " ++ llvmValue w ++ "\n" ++
     prettyLlvm (name+1) (prog result) 
prettyLlvm name (Print z prog) =
  "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 " ++ llvmValue z ++ ")\n" ++
  prettyLlvm name prog
prettyLlvm name Halt = "call void @exit(i32 0)\n" ++
                       "unreachable"
~~~~

We can try this out on a simple program

~~~~{.haskell}
prog0 : AddExp var
prog0 = Lit 3 `Add` (Lit 2 `Add` Lit 4)

llvm0 : Llvm var
llvm0 = compile prog0 (\result => Print result Halt)

renderedLlvm0 : String
renderedLlvm0 = prettyLlvm 1 llvm0
~~~~

    *m> renderedLlvm0
    "%0 = add nsw i32 2, 4\n%1 = add nsw i32 3, %0\ncall i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 %1call void @exit(i32 0)\nunreachable" : String

However this won't quite compile because the functions `printf` and `exit` are
undeclared and there's no main function wrapping the program.  These additions are
a matter of missing header and footer:

~~~~{.haskell}
llvmHeader : String
llvmHeader = "@int_format = private constant [3 x i8] c\"%i\\00\"\n" ++
             "declare i32 @printf(i8* nocapture readonly, ...)\n" ++
             "declare void @exit(i32)\n\n"

llvmMain : String -> String
llvmMain prog = "define i32 @main() {\n" ++
                prog ++
                "\n}"
~~~~

That's everything needed to run the program `renderedLlvm0`, so let's test it out.

    *m> llvmHeader ++ llvmMain renderedLlvm0
    "@int_format = private constant [3 x i8] c\"%i\\00\"\ndeclare i32 @printf(i8* nocapture readonly, ...)\ndeclare void @exit(i32)\n\ndefine i32 @main() {\n%1 = add nsw i32 2, 4\n%2 = add nsw i32 3, %1\ncall i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 %2)\ncall void @exit(i32 0)\nunreachable\n}" : String

Which pretty prints as

~~~~{.llvm}
@int_format = private constant [3 x i8] c"%i\00"
declare i32 @printf(i8* nocapture readonly, ...)
declare void @exit(i32)

define i32 @main() {
%1 = add nsw i32 2, 4
%2 = add nsw i32 3, %1
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 %2)
call void @exit(i32 0)
unreachable
}
~~~~

Placing this in a file `a.ll`, compiling and running gives the expected result

    $ clang a.ll -o a
    $ ./a
    9

As a bonus exemplifying the use of abstract register type `var` in `Llvm`
here's an interpreter for the assembly language

~~~~{.haskell}
evalVal : Value Int -> Int
evalVal (Var z) = z
evalVal (Lit z) = z

eval : Llvm Int -> IO Unit
eval (Add x y prog) = eval (prog (evalVal x + evalVal y))
eval (Print x prog) = do print (evalVal x) 
                         eval prog
eval Halt = pure ()
~~~~

    *m> eval llvm0
    io_bind (io_bind (prim_write "9") (\__bindx => io_pure ()))
            (\__bindx => io_pure ()) : IO ()
