Clafer Instance Generator
=========================

v0.3.2.11-4-2013

[Clafer](http://clafer.org) is a powerful (equivalent to first-order predicate logic) yet lightweight structural modeling language. Despite simplicity and conciseness of Clafer, writing correct models remains challenging due to hard-to-predict interactions among all constraints expressed in the model. **Clafer instance generator** (ClaferIG) is an interactive tool that generates instances and counter examples of concrete clafers in a Clafer model. If the concrete clafers do not have contradicting constraints, the generator produces valid instance data. Otherwise, the generator produces an unsatisfiable core which included all contradicting constraints and generates a counter example by removing one constraint from the core. The generator can potentially produce many instances if the concrete clafers are not fully specialized. The generator produces different instances on-demand. With these capabilities, the instance generator can be used for debugging models: checking the consistency of the model and detecting under- and
overconstraining of the model. The instance generator can also be used programmatically via API (the command line and interactive session interfaces only use the API).

For more information, see [technical report](http://gsd.uwaterloo.ca/node/462).

Getting Clafer Tools
--------------------

Binary distributions of Clafer and ClaferIG for Windows, Mac, and Linux, can be downloaded from [Clafer Tools - Binary Distributions](http://gsd.uwaterloo.ca/node/516). 
Clafer Wiki requires Haskell Platform and MinGW to run on Windows.

In case these binaries do not work on your particular machine configuration, the tools can be easily built from source code, as described below.

The following tools are not part of the binary distribution and they have to be downloaded separately:

* [ClaferMOO](https://github.com/gsdlab/ClaferMooStandalone) is a set of scripts in Python (cross-platform). 
* [ClaferMooVisualizer](https://github.com/gsdlab/ClaferMooVisualizer) is a client/server web application written JavaScript.
* [ClaferConfigurator](https://github.com/gsdlab/ClaferConfigurator) is a client/server web application written JavaScript.

### Dependencies for running

* [Clafer](https://github.com/gsdlab/clafer) v0.3.2
* [Java Platform (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/index.html) v6+, 32bit
* [Alloy4.1](http://alloy.mit.edu/alloy/download.html)

### Installation

1. download the binaries and unpack `<target directory>` of your choice
2. add the `<target directory>` to your system path so that the executables can be found
3. copy Alloy 4.1's jar to the `<target directory>/tools` folder

Integration with Sublime Text 2
-------------------------------

See [IDEs/README.md](IDEs/README.md)

Building & Installation From Source Code
----------------------------------------

### Additional dependencies for building

* [Clafer compiler](https://github.com/gsdlab/clafer) (to produce Alloy models (`.als`)). The version number of the compiler must match the version of the instance generator.
* On Linux, might need to manually install `zlib1g-dev` and `libncurses5-dev` to build one of Haskell packages on which ClaferIG depends
  * on Ubuntu, execute `sudo apt-get install zlib1g-dev libncurses5-dev`

### Important: Branches must correspond

Clafer, ClaferIG, ClaferWiki, ClaferMoo, and ClaferMooVisualizer are following the *simultaneous release model*. 
The branch `master` contains releases, whereas the branch `develop` contains code under development. 
When building the tools, the branches should match:
Releases `clafer/master` and `claferIG/master` are guaranteed to work well together.
Development versions `clafer/develop` and `claferIG/develop` should work well together but this might not always be the case.

### Building

1. install the [Clafer compiler](https://github.com/gsdlab/clafer) into a `<target directory>` of your choice
  * Tip: it is advised to install both tools into the same `<target directory>`
2. in some `<source directory>`, execute `git clone git://github.com/gsdlab/claferIG.git`
3. in `<source directory>/claferIG`, execute
  * `cabal update`
  * `make`

### Installation

1. execute `make install to=<target directory>`
2. add the `<target directory>` is on your command PATH

#### Note: 
> On Windows, use `/` with the `make` command instead of `\`.

Usage
=====

Clafer Instance Generator can be used in interactive and batch modes, as well as, an API.

### Command-line Usage

(As printed by `claferIG --help`)

```
ClaferIG v0.3.2.11-4-2013

igargs [OPTIONS] FILE

Common flags:
     --all=INT           Saves all instances up to the provided scope or a
                         counterexample.
  -s --savedir=FILE      Specify the directory for storing saved files.
     --alloysolution     Convert Alloy solution to a Clafer solution.
  -b --bitwidth=INTEGER  Set the bitwidth for integers.
     --adduidsandtypes   Preserve unique clafer names and add super/reference
                         types in Clafer solution.
  -? --help              Display help message
  -V --version           Print version information
```

`claferIG <model file name>.cfr`

- opens an interactive session and displays the minimal instance or a counterexample

`claferIG <model file name>.cfr -all <scope>`

- opens a non-interactive session and saves all instances up to the provided scope or a counterexample to files named `<model file name>.cfr.<instance number>.data`, one instance per file.

### Interactive Session Usage
In the interactive mode, the users can invoke the following commands by pressing the first letter of the command name or the whole command as marked by boldface:

ClaferIG v0.3.2.11-4-2013

* [tab] - print the available commands or auto-complete command name, a clafer name, or clafer instance name in a given context
* **n**ext or **[enter]** - to produce the next instance if available, a counterexample, or to output a message that no more instances exist within the given scope
* **i**ncrease - to increase the maximum number of instances of a given clafer or all clafers (scope)
* **r**eload - to load a new version of the input model while preserving the current scope settings
* **s**ave - to save all instances displayed so far or a counterexample to files named `<model file name>.cfr.<instance number>.data`, one instance per file
* **q**uit - to quit the interactive session
* **h**elp - to display this menu options summary
* **scope** - to print out the values of the global scope and individual Clafer scopes
* **setUnsatCoreMinimization** - to choose UnSAT core minimization strategy [fastest | medium | best]. Default: fastest
* **claferModel** - to print out the original Clafer model with line numbers and UnSAT core markers
* **alloyModel** - to print out the output of Clafer translator with line numbers and UnSAT core markers
* **alloyInstance** - to print out the Alloy xml document of the most recent solution
* **f**ind - to print a Clafer with given name found in the most recent solution

The command '**i**ncrease' allows you to change the maximum number of instances for a given clafer or for all clafers as follows:

* `i [enter]` - to increase for all clafers by `1` 
* `i <name> [enter]` - to increase for the clafer `<name>` by `1` 
* `i <name> <number>` - to increase for the clafer `<name>` by `<number>`

The command '**f**ind' allows you find a clafer with the given name and print it:

* `f <name>` 

The command **setUnsatCoreMinimization** let's you choose UnSAT core minimization strategy 

* `setUnsatCoreMinimization fastest` - fastest but the worst
* `setUnsatCoreMinimization medium` 
* `setUnsatCoreMinimization best` - best but slowest even for modest size cores


Output format
-------------

### Instance data

The instance data notation is very similar to a regular Clafer notation for concrete clafers with a few differences:

* no constraints
* no types and super types
    * except when `--adduidsandtypes` parameter is used
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
    b$1
    b$2
    c = 10
    d$1 = e1
    d$2 = e2    
    g1 = e1       
        h = 5     
    g2 = e2
        h = 2

e1
    f$1 = 2
    f$2 = 3
    f$3 = 4 
```

### Near-miss instance

Near-miss instance notation is the same as the instance data notation. Additionally, it indicates which constraints belong to the UnSAT Core.

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

Constraints C1, C2, and C3 form an UnSAT Core. Removal of any of them will make the model satisfiable. The constraint C1 is part of the model and cannot be removed (part of domain knowledge). Therefore, either C2 or C3 must be removed to remove the inconsistency. 

On possible near-miss instance:

```clafer
a1
    a
    b
```

Here, `C1` and `C3` are satisfied but `C2` is not. To resolve the conflict and assuming that the counter example is actually a correct instance data, the user has to modify the model by removing `C2`. However, should the counter example actually represent incorrect instance data, the user can remove `C3` to resolve the inconsistency.

How it works
------------

The Clafer instance generator:

* translates the input Clafer model (.cfr) to an Alloy model (.als). The compiler's intermediate representation (IR) contains the mapping between Clafer names and Alloy names. The IR also contains the smallest scopes for each Alloy signature to ensure that a valid instance can be found if it exists
* invokes Alloy Analyzer to produce an instance or find an UnSAT core
** given an UnSAT core, removes constraints from the core until an instance is found - that instance represents the counterexample which violates the removed constraints
* translates the instance or the counterexample data produced by Alloy Analyzer to Clafer instance data format using the name map from IR in a reverse direction,
* for a counterexample, translates the counter example in Alloy to Claefr instance data and constraint violations in Alloy into constraint violations in Clafer model

Need help?
==========

* See [Project's website](http://gsd.uwaterloo.ca/clafer) for news, technical reports and more
* Try live instance of [ClaferWiki](http://gsd.uwaterloo.ca:5001) - it contains many example models.
* Post questions, report bugs, suggest improvements [GSD Lab Bug Tracker](http://gsd.uwaterloo.ca:8888/questions/). Tag your entries with `clafer` (so that we know what they are related to) and with `kacper-bak`, `jimmy-liang`, or `michal` (so that Kacper, Jimmy, or Michał gets a notification).