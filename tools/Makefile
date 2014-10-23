WGET_COMMAND := wget
MNAME := $(shell uname -m | tr "A-Z" "a-z")

ifeq ($(OS),Windows_NT)
	ifeq ($(shell which wget), which: wget: unkown command)
		mingw-get install msys-wget-bin
	endif
	LIB := x86-windows/minisatprover*
else
	UNAME := $(shell uname -s)
	ifeq ($(UNAME), Linux)
		ifeq ($(MNAME), i686)
			LIB := x86-linux/libminisatprover*
		endif
		ifeq ($(MNAME), x86_64)
			# amd64 is a nickname for x86_64
			LIB := amd64-linux/libminisatprover*
		endif
	endif
	ifeq ($(UNAME),Darwin)
		WGET_COMMAND := curl -O
		LIB := x86-mac/libminisatprover*
	endif
endif

ifeq ($(OS),Windows_NT)
	ifeq ($(shell which wget), which: wget: unkown command)
		mingw-get install msys-wget-bin
	endif
else
	UNAME := $(shell uname -s)
	ifeq ($(UNAME),Darwin)
		WGET_COMMAND := curl -O
	endif
endif

all:
	@if test ! -f "alloy4.2.jar"; then \
		echo "[WARNING] Missing alloy4.2.jar. Downloading...";  \
		$(WGET_COMMAND) http://alloy.mit.edu/alloy/downloads/alloy4.2_2014-05-16.jar; \
		mv alloy4.2_2014-05-16.jar alloy4.2.jar; \
	fi