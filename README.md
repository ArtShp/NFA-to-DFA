# NFA-to-DFA

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

## Table of Contents
- [Overview](#overview)
- [User Guide](#user-guide)
    - [Installation](#installation)
    - [Usage](#usage)
- [Developer Guide](#developer-guide)
    - [Project Structure](#project-structure)
    - [Building](#building)
    - [Testing](#testing)
    - [API Reference](#api-reference)
    - [Detailed Library API](#detailed-library-api)
        - [Data Types](#data-types)
        - [Construction](#construction)
        - [Conversion](#conversion)
        - [Simulation & Acceptance](#simulation--acceptance)
- [License](#license)

## Overview
NFA-to-DFA is a Haskell library for converting nondeterministic finite automata (NFA) into equivalent deterministic finite automata (DFA) and for simulating both. It comes with
- Library ([lib/Automata.hs](lib/Automata.hs), [lib/NFA.hs](lib/NFA.hs), [lib/DFA.hs](lib/DFA.hs), ...)
- Executable ([src/Main.hs](src/Main.hs))
- Test suite ([test/Main.hs](test/Main.hs), ...)

## User Guide

### Installation
```sh
# Ensure you have GHC and Cabal 3.x installed
cabal update
cabal build    # builds library, executable & tests
```

### Usage
Import module `Automata` for the library content.
The most important you can find there
- `DFA` and `NFA` definitions
    - `DFA q a`, where `q` - state type (usually `Int`) and `a` - input symbol type (usually `Char`)
    - `NFA q a` — the same
- `makeDFA` and `makeNFA` functions for construction of `DFA` and `NFA`
- `dfaToNfa` and `nfaToDfa` functions that converts a `DFA` to an equivalent `NFA` (and vice versa).
- `dfaIsAccepted` and `nfaIsAccepted` functions to control if the automaton accepts a given word (input)
- `dfaSimulateFromStartWithHistory` and `nfaSimulateFromStartWithHistory` functions to simulate the automaton on a given input with the history of transitions as output

Import module `AutomataTestData` for the test data (some instances of `DFA` and `NFA`). There you can find next instances
- `dfa_1 :: DFA Int Char`
    - A simple DFA that accepts words with even number of 'a'
    - List of possible IO: `dfaIO_1 :: [(String, Bool)]`
- `nfa_1 :: NFA Int Char`
    - Some random instance of NFA
    - List of possible IO: `nfaIO_1 :: [(String, Bool)]`
    - Expected equivalent DFA: `dfaExpected_1 :: DFA (States Int) Char`
- `nfa_2 :: NFA Int Char`
    - A NFA that accepts signed floating-point numbers
    - List of possible IO: `nfaIO_2 :: [(String, Bool)]`
    - Expected equivalent DFA: `dfaExpected_2 :: DFA (States Int) Char`

Example:
```haskell
import Automata
import AutomataTestData as TD

-- Convert an NFA to a DFA
dfa = nfaToDfa TD.nfa_2

-- Check acceptance
dfaIsAccepted dfa "123.45"     -- True
nfaIsAccepted TD.nfa_1 "aba"   -- False

-- Simulate with history
dfaSimulateFromStartWithHistory dfa "aab"
```

## Developer Guide

### Project Structure
```text
.
├── NFA-to-DFA.cabal        # Package description
├── LICENSE                 # MIT license
├── README.md               # Documentation (this file)
├── CHANGELOG.md
├── .gitignore
├── lib/                    # Library source code
│   ├── Automata.hs         # Main library module
│   ├── AutomataTestData.hs
│   ├── AutomataBase.hs
│   ├── DFA.hs
│   ├── NFA.hs
│   ├── Convertions/
│   │   ├── DFAConvertions.hs
│   │   └── NFAConvertions.hs
│   ├── Simulators/
│   │   ├── DFASimulators.hs
│   │   └── NFASimulators.hs
│   └── TestData/
│       ├── TestData.hs
│       ├── DFA_1.hs
│       ├── NFA_1.hs
│       └── NFA_2.hs
├── src/                    # Executable entrypoint
│   └── Main.hs
└── test/                   # Test suite
    ├── Main.hs
    ├── DFA/
    │   ├── DFATests.hs
    │   ├── DFAValidationTests.hs
    │   └── DFASimulatorsTests.hs
    └── NFA/
        ├── NFATests.hs
        ├── NFAValidationTests.hs
        ├── NFASimulatorsTests.hs
        └── NFAConvertionsTests.hs
```

### Building
```sh
# Build library, executable and tests
cabal build
```

### Testing
```sh
# Run all HUnit tests
cabal test
```

Test modules include `test/DFA/DFATests.hs` and `test/NFA/NFATests.hs` test groups.

### API Reference
- `Automata` module: core definitions
- `DFA` and `NFA` modules: data types and parsing
- Convertions functions in `Convertions.DFAConvertions` and `Convertions.NFAConvertions`
- Simulators in `Simulators.DFASimulators` and `Simulators.NFASimulators`
- Test data in `AutomataTestData`

For detailed signatures, open the modules above.

### Detailed Library API

#### Data Types
```haskell
type States q = Set.Set q
type Alphabet a = Set.Set a
```

```haskell
type DFATransition q a = Map.Map (q, a) q

data DFA q a = DFA
    { dfaStates       :: States q
    , dfaAlphabet     :: Alphabet a
    , dfaTransition   :: DFATransition q a
    , dfaInitialState :: q
    , dfaFinalStates  :: States q
    } deriving (Eq)
```

```haskell
type NFATransition q a = Map.Map (q, Maybe a) (States q) -- `Nothing` represents ε-move

data NFA q a = NFA
    { nfaStates       :: States q
    , nfaAlphabet     :: Alphabet a
    , nfaTransition   :: NFATransition q a
    , nfaInitialState :: q
    , nfaFinalStates  :: States q
    } deriving (Eq)
```

#### Construction
Constructors produce an automaton if its definition is valid.

```haskell
makeDFA :: (Ord q, Ord a) => States q -> Alphabet a -> DFATransition q a -> q -> States q -> Maybe (DFA q a)
makeNFA :: (Ord q, Ord a) => States q -> Alphabet a -> NFATransition q a -> q -> States q -> Maybe (NFA q a)
```

#### Conversion
- `dfaToNfa`  
  Embeds a DFA as an equivalent NFA (adds no ε-moves).

- `nfaToDfa`  
  Subset‐construction algorithm:  
  1. Start from ε-closure of the NFA start state.  
  2. For each unprocessed DFA state `S`, and each symbol `a`, compute the ε-closure of `move(S, a)`.  
  3. Repeat until no new states.

```haskell
dfaToNfa :: (Ord q, Ord a) => DFA q a -> NFA q a
nfaToDfa :: (Ord q, Ord a) => NFA q a -> DFA (States q) a
```

#### Simulation & Acceptance
- `dfaIsAccepted`
    Follows the unique transition path and tests membership in `accepts`

- `nfaIsAccepted`
    Returns `True` if any sequence of moves (including ε) leads to an accept state

- `dfaSimulateFromStartWithHistory`  
    Runs the DFA on the input word, returning  
    1. a list of transitions `(currentState, symbol, nextState)`  
    2. the final state reached

- `nfaSimulateFromStartWithHistory`  
    Runs the NFA on the input word, returning a set of all possible runs. Each run is:  
    1. a transition history `[(state, Maybe symbol, nextState)]` (with `Nothing` for ε-moves)  
    2. the final states reached

```haskell
dfaIsAccepted :: (Ord q, Ord a) => DFA q a -> [a] -> Bool
dfaSimulateFromStartWithHistory :: (Ord q, Ord a) => DFA q a -> [a] -> ([(q, a)], Maybe q)

nfaIsAccepted :: (Ord q, Ord a) => NFA q a -> [a] -> Bool
nfaSimulateFromStartWithHistory :: (Ord q, Ord a) => NFA q a -> [a] -> Set.Set ([(q, Maybe a)], States q)
```

## License
© 2025 Artemiy Shipovalov  
Released under the [MIT License](LICENSE).
