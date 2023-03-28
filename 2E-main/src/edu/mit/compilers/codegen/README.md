# High level overview

## Destruct and ShortCircuit

Destruct has various functions to help `destruct` the AST into a `CFG`. ShortCircuit has various functions to be able to `shortcircuit` boolean-evaluating expressions into `CFG`s with outputs (NOTE that these ONLY apply to expressions **inside a condition statement**; a boolean expression used anywhere else is simply destructed).

## Types

This will be updated soon. It's trying to be like LLVM but maybe without SSA. We are going to want an easy way
to simplify our code. Registers (virtual registers) are just a way of keeping track of different locations (i.e.
variables, temps, etcetera). Because of this, there should be a lot of opportunity to delete a lot of code. To
keep track of different locations all we need to know is (1) the name of the location, and (2) the type of the location
(to a minor extent).

ASTBlocks are sequences (lists) of lines with parents and children. They are meant to be merged later on.

NOTE: all these have to-string. **THERE IS A KNOWN BUG WHERE TO-STRING FAILS TO PRINT ALL THE BLOCKS**. This bug occurs (probably) because the parents/children are not properly set (and/or the nodes in the CFG are not proparly added to that set).

## ControlFlow

Here we have functions that create CFGs from methods and create CFGManagers (objects which have many CFGs and globals in a program) for a single program.

## CodeGen

This provides a function that generates code recursively for the CFG. The ids of the temporary (addresses) will usually be how far they are down in the stack (multiplied by 8 or 16 bytes).

# What Needs to be done

Seperate allocation and optimization from flattening. These should all probably be implementations of "transformations" which are basically functions which take in a CFG and return a new CFG.