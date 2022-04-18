# README.md

## Introduction

### Testing the Interpreter against all test programs

To compile run `stack build` and to run all tests do 

    stack test 
    
To test a particular `program.cc` run

    stack build
    stack exec CPPInterpreter-exe ./test/good/program.cc
    
### Testing a  particular program

Testing particular programs is important, if the interpreter does not give the right result on a particular `program.cc` (such as, for example, looping infinitely). The way forward then is to **write your own test programs**. For example,

- make  `program.cc` so small that the error does not occur anymore
- make  `program.cc` bigger so that the error comes back
- iterate the above until you found the smallest version of `program.cc` that exhibits the bug
- insert `printInt` statements as needed to track the execution of the program
- guess what could cause the problem and change `Interpreter.hs`
- iterate all of the above

The reason why debugging the interpreter is more difficult than debugging the typechecker is that the interpreter is not purely compositional in the abstract syntax. For example, even if it looks as if a wrong interpretation of `SWhile` might be responsible for an infinite loop, the problem could also lie in any number cof the other cases (eg `Sif`, `SDecl`, `SInit`, `EAss`, etc) as they all change the environment and therefore can change the values of the variables involved in testing the condition responsible for entering the `while`.

If all of this does not help, save this particular program for the end and try to get the interpreter first working on all of the other programs. If you are lucky, this will already solve the problem of `program.cc` as well.

## Assignment Interpreter  

Don't forget the Haskell Tips. 

To decouple the interpreter from the typechecker, we implement the interpreter independently of the typechecker. (Just don't be surprised if curious things happen if you run the interpreter on "bad" programs.)

Test your interpreter on all (good) test files.

### Part 1

- Run `stack build` and then `stack test` (or just `stack test`). Among other output, you should see

      Good programs: passed 10 of 75 tests

  To see which are the good programs run `stack test | grep OK`. Look at these programs in `test/good/` to get an idea of what the interpreter can already do and study the functions `evalStm` and `evalExp` in `src/Interpreter.hs`. Try to understand the grand outlines of how these two functions interpret the 10 passing programs.

- Look at the program `easy_add.cc`. Draw the abstract syntax tree in your mind or using bnfc. Which missing case of the `evalExp` do you need to implement in order for the interpreter to work on this program?

- Extend `Interpreter.hs` so that it interpretes correctly `easy.add`. Hint: Study the case of `ETimes` that is already implemented.

- Commit and push your changes to git and notify me by email.

- Do as many of the other expressions you can.

### Remark: Comparing Two Implementations of the Interpreter

You will work with `Interpreter.hs` but to make the comparison with the operational semantics from the book easier, I briefly also discuss `Interpreter_v_gamma.hs`. The former has, for example, 

```haskell
evalStm (SReturn e) = do
    v <- evalExp e
    return $ Just v
```

while the latter has

```haskell
evalStm env (SReturn e) = do
    (v, env') <- evalExp env e
    return (Just v, env')
```

The second is closer to the operational semantics where `(v, env')` directly corresponds to the <v,gamma'> of the operational semantics. The first is more concise since it leaves the environment implicit. Where `Interpreter_v_gamma.hs` has in line 24

```haskell
class Monad i => Interpreter i where
```

`Interpreter.hs` builds the environment into the monad (a so-called state monad because it contains "state" (namely the environment))

```haskell
class MonadState Env i => Interpreter i where
```

one advantage of this is that the rule we called "propagation of side effects" in the lecture is implemented automatically by the state monad. 

## Part 2

Finish the interpreter. 