# PrologInterpreter

This is a simple Prolog interpreter implemented in Haskell. It parses Prolog source files, evaluates queries via a top‑down strategy, and supports unification and backtracking.

## Table of Contents

1. [Overview](#overview)  
2. [Features](#features)  
3. [Repository Structure](#repository-structure)  
4. [Getting Started](#getting-started)  
   1. [Requirements](#requirements)  
   2. [Build & Run](#build--run)  
   3. [Usage](#usage)  
5. [Core Modules](#core-modules)  
6. [Testing](#testing)  
7. [Limitations & Future Work](#limitations--future-work)  
8. [License](#license)  
9. [Acknowledgments](#acknowledgments)  

## Overview

This interpreter reads a Prolog file containing facts and rules, accepts queries interactively (or from input), and attempts to resolve them using unification and a depth‑first top-down evaluator.  

It’s intended for educational purposes: to illustrate how Prolog evaluation works under the hood.

## Features

- Lexical analysis (tokenization) of Prolog source  
- Parsing into an AST (Abstract Syntax Tree)  
- Syntax validation  
- Unification algorithm producing most general unifiers (MGUs)  
- Application of substitutions to queries and predicates  
- Top-down (depth-first) evaluation with backtracking  
- Basic handling of variable scoping (renaming of variables internally)  
- A test suite for the unification algorithm  
- Simple command-line interface (read file, accept queries, quit via `halt.`)  

## Repository Structure

```
PrologInterpreter/
│  
├── LICENSE  
├── Makefile  
├── main.hs  
├── PrologLexer.hs  
├── PrologParser.hs  
├── PatternMatch.hs  
├── TopDownEval.hs  
├── mgutests.hs  
├── tests/  
│   ├── *.pl              ← sample Prolog files  
│   └── queriesAndOutputs.txt  
└── README.md  
```

Brief descriptions:

- **main.hs** — Entry point. Loads a Prolog file, then loops to read and evaluate queries until `halt.`  
- **PrologLexer.hs** — Converts source text into tokens  
- **PrologParser.hs** — Parses tokens into AST nodes, with syntax checking  
- **PatternMatch.hs** — Implements `unify`, `applyMgu`, `composeMgu`  
- **TopDownEval.hs** — Contains the top-down evaluation logic with backtracking  
- **mgutests.hs** — Tests for the unification / MGU logic  
- **tests/** — Sample Prolog files and a mapping of queries → expected outputs  

## Getting Started

### Requirements

- GHC (Glasgow Haskell Compiler)  
- `make` (or equivalent build tool)  

### Build & Run

You can build and run the interpreter via the provided `Makefile`:

```bash
# compile the interpreter and test suite
make

# run the interpreter using a Prolog source file
./main path/to/file.pl
```

To clean up build artifacts:

```bash
make clean
```

### Usage

1. Start the interpreter with your Prolog source file:  
   ```
   ./main myprogram.pl
   ```

2. At the prompt, type a Prolog query, for example:
   ```
   ancestor(X, Y).
   ```

3. The interpreter will either print a solution (e.g. `X = alice, Y = bob.`) or `fail.` if none.

4. To stop, enter:
   ```
   halt.
   ```

## Core Modules Explained

### PrologLexer.hs  
Tokenizes source input into identifiers, symbols, punctuation, variables, etc.

### PrologParser.hs  
Transforms tokens into AST nodes: facts, rules, predicates, variables, compound terms. Also checks syntactic validity.

### PatternMatch.hs  
- `unify(term1, term2)` finds a substitution (MGU) if possible  
- `applyMgu` applies a substitution to terms or predicates  
- `composeMgu` merges multiple substitutions  

### TopDownEval.hs  
Implements resolution via:
- Selecting a goal from the query
- Matching it against facts or rule heads
- Substituting and recursing on body goals
- Backtracking when a branch fails

The interpreter also handles variable name clashes by internally renaming variables in rules with apostrophes.

## Testing

- `mgutests.hs` runs unit tests for unification and MGU logic  
- In the `tests/` directory, Prolog files along with a `queriesAndOutputs.txt` provide sample runs you can manually verify  
- You can extend the test suite by adding more `.pl` files and corresponding expected results

## Limitations & Future Work

- Does not support advanced Prolog features like:
  - Cut (`!`)
  - Negation as failure (`\+`)
  - Built-in predicates or arithmetic
  - Lists and the full term unification complexity  
- No inference optimizations (like tabling or iterative deepening)  
- No GUI or web interface  
- The evaluation strategy is simple depth-first; may loop on certain recursive definitions  
- Variable scoping is rudimentary (apostrophe-based renaming), may need refinement  

Possible extensions:
- Add support for built-ins (e.g. arithmetic, comparisons)  
- Implement `cut`, negation, or other control constructs  
- Add a REPL with query history  
- Implement optimizations (memoization, iterative deepening)  
- Better error messages (syntax and runtime)  

## License

This project is released under the **MIT License**. See the `LICENSE` file for details.
