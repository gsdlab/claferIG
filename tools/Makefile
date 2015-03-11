WGET_COMMAND := wget

ifneq ($(OS),Windows_NT)
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
