Clafer Instance Generator
=========================

Overview
--------

**Clafer instance generator** (claferIG) is an API and an interactive tool that generates instances and counter examples of concrete clafers in a Clafer module. If the concrete clafers do not have contradicting constraints, the generator produces valid instance data. Otherwise, the generator produces a counter example that illustrates the constraint violation. The generator can potentially produce many instances if the concrete clafers are not fully specialized. The generator first returns the minimal instance and subsequently produces instances of increasing size, on-demand. The instance generator can also be used directly as an API (the command line and interactive session interfaces only use the API).

Dependencies
------------

* [Clafer translator](https://github.com/gsdlab/clafer) (to produce Alloy models (.als) and Clafer IR (.xml) from Clafer models)
* [Alloy 4.2](http://alloy.mit.edu/alloy/) (backend reasoner)
* MiniSAT Proover (a SAT solver used by Alloy that can produce UnSAT Core, bundled with Alloy)
* Some Haskell libraries (installed automatically by Cabal)

### Prerequisites
1. install the Haskell Platform (it contains GHC and Cabal)
2. install the [Clafer translator](https://github.com/gsdlab/clafer) into a `<target directory>` of your choice
3. make sure the `<target directory>` is on your command PATH

### Building

1. execute `git clone git://github.com/gsdlab/claferIG.git`
2. copy `alloy4.jar` into the newly created `claferIG` directory
3. execute `make`

#### Note: 
> On Windows, install Cygwin with the `make` package.

### Installation

1. execute `make deploy to=<target directory>`

Usage
-----

Clafer Instance Generator can be used in interactive and batch modes, as well as, an API.

### Command-line Usage

`claferIG <model file name>.cfr`

- opens an interactive session and displays the minimal instance or a counterexample

`claferIG <model file name>.cfr -all <scope>`

- opens a non-interactive session and saves all instances up to the provided scope or a counterexample to files named `<model file name>.cfr.<instance number>.data`, one instance per file.


### Interactive Session Usage
In the interactive mode, the users can invoke the following commands by pressing the first letter of the command name or the whole command as marked by boldface:

* [tab] - print the available commands or auto-complete command name, a clafer name, or clafer instance name in a given context
* **n**ext or **[enter]** - to produce the next instance if available, a counterexample, or to output a message that no more instances exist within the given scope
* **i**ncrease - to increase the maximum number of instances of a given clafer or all clafers (scope)
* **r**eload - to load a new version of the input model while preserving the current scope settings
* **s**ave - to save all instances displayed so far or a counterexample to files named `<model file name>.cfr.<instance number>.data`, one instance per file
* **q**uit - to quit the interactive session
* **h**elp - to display this menu options summary
* **scope** - to print out the values of the global scope and individual Clafer scopes
* **resetScopes** - to load the original minimal scopes computed by the Clafer translator
* **claferModel** - to print out the original Clafer model with line numbers and UnSAT core markers
* **alloyModel** - to print out the output of Clafer translator with line numbers and UnSAT core markers
* **alloyInstance** - to print out the Alloy xml document of the most recent solution
* **f**ind - to print a Clafer with given name found in the most recent solution

The command '**i**ncrease' allows you to change the maximum number of instances for a given clafer or for all clafers as follows:

* `'i' [enter]` - to increase for all clafers by `1` 
* `'i' <name> [enter]` - to increase for the clafer `<name>` by `1` 
* `'i' <name> <number>` - to increase for the clafer `<name>` by `<number>`

The command '**f**ind' allows you find a clafer with the given name and print it:

* `'f' <name>` 

Output format
-------------

### Instance data

The instance data notation is very similar to a regular Clafer notation for concrete clafers with a few differences:

* no constraints
* no types and super types
* no clafer and group cardinalities (each clafer has the default group `(0..*)` and clafer `(1..1)` cardinality)
* no clafers not present in the instance

Additionally, the data notation contains concrete values of the clafers and suffix numbers to distinguish among multipe instances of the same clafer.

### Note:
> The instance data models could be read by the Clafer translator if the translator had simple type inference support.

#### Example 

For a model

```clafer
abstract A
    a ?
    b +
    c : integer ?
    d -> E 2
    g -> E 2
        h : integer

abstract E
    f : integer +

a1 : A
e1 : E
e2 : E
```

A possible instance data looks as follows:

```clafer
a1
    b1
    b2
    c = 10
    d = e1, e2    // concise reference notation - no children
    g1 = e1       // expanded reference notation - with children
        h = 5     
    g2 = e2
        h = 2

e1
    f = 2, 3, 4   // concise multivalue notation
```

### Counter example

The counter example notation is the same as the instance data notation. Additionally, it indicates which constraints belong to the UnSAT Core.

#### Example 

For a model

```clafer
abstract A
    a ?
    b ?
        [ a ]   // C1

a1 : A
    [ no a ]    // C2
    [ b ]       // C3
```

Constraints C1, C2, and C3 form an UnSAT Core. Removal of any of them will make the model satisfiable. The constraint C1 is part of the model and cannot be removed (part of domain knowledge). Therefore, either C2 or C3 must be removed to remove the inconsistency. The command `claferModel` prints the model as follows:

```clafer
1. |abstract A
2. |    a ?
3. |    b ?
4. >        [ a ]   <--- UnSAT Core
5. |
6. |a1 : A
7. >    [ no a ]    <--- UnSAT Core
8. >    [ b ]       <--- UnSAT Core
```

On possible counter example that illustrates the inconsistency is as follows:

```clafer
a1
    a
    b
```

Here, `C1` and `C3` are satisfied but `C2` is not. To resolve the conflict and assuming that the counter example is actually a correct instance data, the user has to modify the model by removing `C2`. However, should the counter example actually represent incorrect instance data, the user can remove `C3` to resolve the inconsistency.

How it works
------------

The Clafer instance generator:

* translates the input Clafer model (.cfr) to an Alloy model (.als) and Clafer IR model (.xml). The IR model contains the mapping between Clafer names and Alloy names. The IR also contains the smallest scopes for each Alloy signature to ensure that a valid instance can be found if it exists
* invokes Alloy Analyzer to produce an instance or find an UnSAT core
** given an UnSAT core, removes constraints from the core until an instance is found - that instance represents the counterexample which violates the removed constraints
* translates the instance or the counterexample data produced by Alloy Analyzer to Clafer instance data format using the name map from IR in a reverse direction,
* for a counterexample, translates the counter example in Alloy to Claefr instance data and constraint violations in Alloy into constraint violations in Clafer model

Need help?
----------
Post questions, report bugs, suggest improvements [GSD Lab Bug Tracker](http://gsd.uwaterloo.ca:8888/questions/). Tag your entries with `claferig` (so that we know what they are related to) and with `jimmy-liang` (so that Jimmy gets a notification).