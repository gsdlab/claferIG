##### ClaferIG Version 0.5.5, unreleased

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
