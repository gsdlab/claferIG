##### ClaferIG Version 0.5.7, unreleased

Minor release with Clafer 0.5.7.

* The UIDs of the clafers are their names since Clafer 0.5.7, qualified only where a name is not unique (`x__size`), and the Alloy model renames those that cannot be Alloy names (e.g., `sig_1` for `sig`). claferIG shows the names of the clafers, also with `--useuids` (their UIDs, not their Alloy names), and takes UIDs in its commands and scope files (`i sig 3`, `saveScopes`, `loadScopes`); it translates between the UIDs and the Alloy names. It cut a sig's name after its first `_` when a clafer was not found.
* Dynamic models: the obligations of a constraint last while the parent of its context exists, so a transition nested in its source state, e.g., `[ req=down --> movingDown ]` in the state `movingUp`, takes the machine to its target ([clafer#107](https://github.com/gsdlab/clafer/issues/107)).
* Dynamic models: a negated obligation lapses with the parent too, so `[ !(F error) ]` lets the parent disappear, like `[ G !error ]` ([clafer#110](https://github.com/gsdlab/clafer/issues/110)).
* Unsat cores have the constraints of abstract clafers and of their children, each at its position in the model ([#46](https://github.com/gsdlab/claferIG/issues/46)). Alloy gave the position of the whole signature for a constraint of a signature that is not a `one sig`, which matched no constraint of the model: such constraints were left out of the core, and a model whose constraints were all in abstract clafers gave "No more instances found" without a core.
* The next trace, `fork` and `config` are solved without logging the translation for an unsat core, as `check` was: they give no core. The log of a large model took gigabytes, e.g., 11 GB in 3 minutes for the next trace of the TLS handshake model, and filled the temporary folder of a session; the first solution, which gives the core of a model with no instances, still logs it.
* The next trace, `fork` and `config` differ from the traces given so far in what the traces show. The reference of a clafer that is not in a state has any value there, and a trace that differed from the last one only in such a value was given as a new trace that looked the same, e.g., the same trace of the WebSocket model again and again.
* An abstract clafer that no concrete clafer inherits from, and the clafers in it, have no instances: their scopes stay 0 when the scopes are raised, with `i`, `i <clafer>`, `s <clafer> <number>`, a `.cfr-scope` file or a reload, and `i <clafer>` and `s <clafer> <number>` say so. Raising them gave Alloy atoms of such clafers, which made a model, e.g., the power window with abstract variants of its controller, take minutes instead of seconds.
* Ending a session no longer prints a `NoSuchFileException` when Alloy removes its own temporary files while claferIG removes the session's folder.
* The model tests check the scopes of the patterns `must follow ... between` and `must precede ... after`, and that `finaltarget` lets the instances of a clafer swap their targets, while `finalref` and `final` do not ([kbak/clafer-semantics#127](https://github.com/kbak/clafer-semantics/issues/127)).
* The model tests run in parallel, on all cores, each in a folder of its own (two tests wrote different `lib.cfr` files into one folder); the lifetimes, pattern scope and `finaltarget` tests are split into one test for each case. `make test` takes about 30 seconds instead of 2 minutes.
* The model tests check the lifetimes of obligations, the identities in a scope and the parent of a reappearing instance, with both scope strategies, at the scopes 1 to 3 and up to 3 and 10 states, and that the scopes of abstract clafers without instances stay 0.

##### ClaferIG Version 0.5.6, Nov 1, 2026

Minor release with Clafer 0.5.6.

* Dynamic models, with temporal operators or the temporal modifiers `initial` and `final`: their solutions are traces, whose states are instances. `n` gives the next solution, an instance of a static model or a trace of a dynamic model, with its states (`--- State 0 ---`) and the state to which it loops back (`--- Loop back to state 3 ---`). An instance has the same name in all the states of a trace. claferIG refused dynamic models.
* The traces are given from the shortest, each different from the traces given before, regardless of the names of their atoms, up to the maximum trace length: `--tl` and the command `tl <number>` (by default, 10 states). Alloy 6.2 enumerates only a few traces of a temporal model, or none, so claferIG solves for each next trace without the traces given before.
* Diff view: `--diff`, or the command `d`, shows the first instance or state with `+` on each line, and each next one as its changes from the previous one: the clafers that appear (`+`), with their children, that disappear (`-`), and the values that change (`~`), each indented as in the tree and with its least-partially-qualified name, e.g., `normal.yellow`. The instances of a static model are compared by their structure.
* `--all` saves each trace to its own file, `--json` gives a trace as `{"states": [...], "loop": 3}`, and `f` finds a clafer in all the states of the most recent trace.
* `fork <number>` gives another trace with the same states as the last trace before the given state, and `config` a trace whose clafers that do not change over time, e.g., `final` features, differ from those of the traces given so far.
* The command `config` also works on static models: an instance whose structure, its clafers without the values of their references, differs from those of the instances given so far.
* The command `check` checks the assertions of the model within the scopes and the maximum trace length, and gives a counterexample of each one that fails; `check <number>` checks only the assertions on that line ([#30](https://github.com/gsdlab/claferIG/issues/30)). claferIG ignored assertions.
* The command `dot` saves the most recent solution as a Graphviz graph, for a trace its states and transitions, and `--dot` gives the solutions as such graphs. In the diff view, the states of the graph show their changes.
* Unsat cores: a model with no instances, or a dynamic model with no traces, gives the constraints that contradict each other and a near-miss example. claferIG gave no unsat core when Alloy found the model unsatisfiable without solving, e.g., for `[ some b ]` and `[ no b ]`, and for dynamic models. The constraints are shown with the right text when their lines are indented with tabs, and with the parentheses that close them, e.g., `initially (green --> red)`.
* `-n` limits the number of instances that `--all` saves, as chocosolver's `-n` does, e.g., `claferIG --all -n 2 model.cfr` to check whether a model has exactly one instance ([#43](https://github.com/gsdlab/claferIG/issues/43)).
* Each session keeps the temporary files of Alloy in a folder of its own, which is removed when it ends, also when claferIG is killed. Kodkod's logs stayed in the temporary folder of the system, and those of large models filled it.

##### ClaferIG Version 0.5.5, Oct 27, 2026

Minor release with Clafer 0.5.5.

* A model that Alloy does not support, e.g., with decimals, reals or doubles, is refused with the compiler's reason ("Alloy output unavailable because the model contains: a decimal literal, ..."); claferIG crashed with "No match in record selector extension". Decimals are supported by the Choco backend (chocosolver) only.
* The errors before "Press enter to retry." are flushed, so a tool reading claferIG's output through a pipe, e.g., ClaferIDE, shows them; it showed nothing and waited.
* A `.cfr-scope` file may list only some clafers, without the global scope (`"lpqName": ""`), e.g., `[{"lpqName": "Component::Cfeatures", "scope": 132}]`: `loadScopes` keeps the global scope and `--all` uses the given one. Such a file crashed claferIG with "Prelude.maximum: empty list". chocosolver 0.5.5 reads the same files.

##### ClaferIG Version 0.5.4, Oct 25, 2026

Minor release with Clafer 0.5.4.

* Models in several files: `--import=FILE`, in the model's `//# OPTIONS` or on the command line, generates instances of the model together with the imported files, as with the Clafer compiler ([gsdlab/clafer#100](https://github.com/gsdlab/clafer/issues/100)). The model and its imports can be URLs; the saved files are named after a URL's last segment.
* `--import-root=DIR`, `--public-imports` and `--import-base=URL` restrict the imports of a model that may come from anyone, as with the compiler (e.g., in a web tool); they can only be given on the command line.
* The options in the model's `//# OPTIONS` directive apply, as they do with the compiler: the layout, the scope strategy, and the other options of the compilation. The integer range of `--maxint` and `--minint` raises the bitwidth when it needs a larger one.
* Positions are given as `file.cfr:line:column`, the file in which they are, also in a model of one file: errors are reported as by the compiler (`Parse failed at model.cfr:3:5...`, `Compile error at ...`, `Error...`, then the message), and the constraints of an unsatisfiable core give their position (`x < 2 (model.cfr:4:7)`, previously `(line 4, column 7)`). The command `c` (`claferModel`) shows each file under its name, with the numbers of its lines.
* The command `c` (`claferModel`) shows the scope of the model's first clafer; its line had no scope, and the line of its first child showed the first clafer's scope instead of the child's.
* Instances show the clafers' names from the model instead of names derived from their UIDs, which differ for names with apostrophes (`same'` has the UID `c0_same__prime__`). This also applies to the clafer names in scope messages.
* [Release](https://github.com/gsdlab/claferIG/pull/38)

##### ClaferIG Version 0.5.3, Oct 15, 2026

Minor release with Clafer 0.5.3 and Alloy 6.2.0.

* Uses Alloy 6.2.0 (`alloy6.2.0.jar`, Java 17+) instead of Alloy 4.2. The MiniSat solver with UnSAT cores is part of the Alloy jar on Linux and macOS (Intel and ARM); on Windows there is none, so instances come from SAT4J and there are no UnSAT cores.
* The MiniSat solver in Alloy 6.2.0 is built without optimization and is several times slower ([AlloyTools/org.alloytools.alloy#352](https://github.com/AlloyTools/org.alloytools.alloy/issues/352)). `make` builds it with optimization into `lib/` (needs `cmake`), and ClaferIG uses that build when it is next to the executable.
* Works around an Alloy 6.2.0 bug that made every UnSAT core empty (the MiniSat prover is freed before its proof is read; fixed in Alloy after 6.2.0).
* The tests that need the example models and Alloy run in the `model-tests` suite (flag `model-tests`, off by default) and are not part of the package.
* [Release](https://github.com/gsdlab/claferIG/pull/37)

##### ClaferIG Version 0.5.2 released on Oct 9, 2026

* [Release](https://github.com/gsdlab/claferIG/pull/36)

##### ClaferIG Version 0.5.1 released on Sep 12, 2023

* [Release](https://github.com/gsdlab/claferIG/pull/35)

##### ClaferIG Version 0.4.5 released on Mar 13, 2017

* [Release](https://github.com/gsdlab/claferIG/pull/33)

##### ClaferIG Version 0.4.4 released on Jun 23, 2016

* [Release](https://github.com/gsdlab/claferIG/pull/32)

##### ClaferIG Version 0.4.3 released on Dec 22, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/31)

##### ClaferIG Version 0.4.2.1 released on Oct 19, 2015

* Fixed Java manifest file, added stack instructions to README

##### ClaferIG Version 0.4.2 released on Oct 16, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/28)

##### ClaferIG Version 0.4.1 released on Sep 1, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/27)

##### ClaferIG Version 0.4.0 released on Jul 28, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/26)

##### ClaferIG Version 0.3.10 released on April 24, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/25)

##### ClaferIG Version 0.3.9 released on March 06, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/24)

##### ClaferIG Version 0.3.8 released on January 27, 2015

* [Release](https://github.com/gsdlab/claferIG/pull/23)

##### ClaferIG Version 0.3.7 released on October 23, 2014

* [Release](https://github.com/gsdlab/claferIG/pull/22)

##### ClaferIG Version 0.3.6.1 released on July 08, 2014

* [Release](https://github.com/gsdlab/claferIG/pull/20)

##### ClaferIG Version 0.3.6 released on May 23, 2014

* [Release](https://github.com/gsdlab/claferIG/pull/19)

##### ClaferIG Version 0.3.5 released on January 20, 2014

* [Release](https://github.com/gsdlab/claferIG/pull/17)

##### ClaferIG Version 0.3.4 released on September 20, 2013

##### ClaferIG Version 0.3.3 released on August 14, 2013

* [Release](https://github.com/gsdlab/claferIG/pull/12)

##### ClaferIG Version 0.3.2 released on April 11, 2013

##### ClaferIG Version 0.3.1 released on October 17, 2012

##### ClaferIG Version 0.3 released on July 17, 2012

This was the first release of Clafer Instance Generator and included all code since the beginning of the project.
