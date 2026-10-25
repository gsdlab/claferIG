##### ClaferIG Version 0.5.4, unreleased

Minor release with Clafer 0.5.4.

* Instances show the clafers' names from the model instead of names derived from their UIDs, which differ for names with apostrophes (`same'` has the UID `c0_same__prime__`). This also applies to the clafer names in scope messages.

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
