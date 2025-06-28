# Revision history for NFA-to-DFA

## 0.1.0.0 -- 2025-06-27

* Add DFA and NFA definitions
* Add NFA-to-DFA and DFA-to-NFA convertions
* Add unit tests skeleton

## 0.2.0.0 -- 2025-06-27

* Add DFA and NFA simulators

## 0.3.0.0 -- 2025-06-27

* Fix NFA simulators with history output
* Add DFA and NFA unit tests, that cover simple cases

## 0.4.0.0 -- 2025-06-28

* Change library structure:
    * Two modules are exposed: `Automata` and `AutomataTestData`
* Change API for DFA and NFA
    * Their data getters have distinct names
* Implement some basic usage of library in Main
* Introduce some test data
* Fix bug with DFA history
* Add show methods for DFA and NFA
