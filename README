Overview:

Clafer instance generator (claferIG) is an API and an interactive tool that generates instances and counter examples of concrete clafers in a Clafer module. If the concrete clafers do not have contradicting constraints, the generator produces valid instance data. Otherwise, the generator produces a counter example that illustrates the constraint violation. The generator can potentially produce many instances if the concrete clafers are not fully specialized. The generator first returns the minimal instance and subsequently produces instances of increasing size, on-demand. The instance generator can also by used directly as an API (the command line and interactive session interfaces only use the API).

Dependencies:

* Clafer translator (to produce Alloy models from Clafer models and name traceability map)
* Alloy 4.2 (backend reasoner)
* MiniSAT (SAT solver used by Alloy that can produce Unsat Core)

Command-line Usage:

claferIG <model file name>.cfr
- opens an interactive session and displays the minimal instance or a counterexample

Interactive Session Usage:

- 'n' - produces the next instance if available or outputs a message that no more instances exist
- 's' - saves all instances or a counterexample to a file <model file name>.cfr.data
- 'q' - quits the interactive session

Output format:

* Instance data
The instance data notation is very similar to a regular Clafer notation for concrete clafer with a few differences:
- no constraints
- no types and super types
- no clafer and group cardinalities (each clafer has the default group
(0..*) and clafer (1..1) cardinality)
- no clafers not present in the instance

Additionally, the data notation contains concrete values of the clafers.

* Counter example
The counter example notation is the same as the instance data notation. Additionally, it indicates which constraints belong to the UnSAT Core.

How it works:

The Clafer instance generator:
* translates the input Clafer model to an Alloy model and produces a name traceability map which maps names of clafers and constraints to the corresponding names and constraints in Alloy,
* computes the smallest scopes for each Alloy signature to ensure that a valid instance can be found if it exists,
* invokes Alloy Analyzer to produce an instance or find a counterexample,
* translates the instance or the counterexample data produced by Alloy Analyzer to Clafer instance data format using the name traceability map in a reverse direction,
* for a counterexample, translates constraint violations in Alloy into constraint violations in Clafer model.
