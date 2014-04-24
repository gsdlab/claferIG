ClaferIG uses Alloy which, in turn, requires a SAT solver. ClaferIG requires support for UnSAT core computation, which is provided by `minisatproover` library. 

In this folder, you need to have an appropriate `minisatproover` library for your operating system. 
When building from source code, this gets created for you by the `tools/Makefile`.
When building from Hackage, you need to extract the library from `alloy.jar`. 

The taget folder structure should be

1. for 32bit Linux

`<cabal>/bin/lib/x86-linux/libminisatprover*`

2. for 64bit Linux

`<cabal>/bin/lib/amd64-linux/libminisatprover*`

3. for Windows

`<cabal>/bin/lib/x86-windows/minisatprover*`

4. for Max

`<cabal>/bin/lib/x86-mac/libminisatprover*


