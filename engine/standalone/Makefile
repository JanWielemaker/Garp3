################################################################
# Makefile for garp on Unix systems
# Author: Jan Wielemaker
#
# Requires: SWI-Prolog 2.7.x or later
################################################################

SHELL=/bin/sh
MAKE=make

PL=pl
COFLAGS=-O2

DEFS=
CC=gcc
CFLAGS=-fPIC $(COFLAGS) $(DEFS) -I$(PLBASE)/include
LD=gcc
LDFLAGS=-shared
ARCH=sparc-sunos-4.1.3

prefix=/usr/local
exec_prefix=/usr/local
bindir=$(exec_prefix)/bin
libdir=$(prefix)/lib
garpdir=$(libdir)/garp
BITS=$(ARCH)/pl-bit.so
PLVARS=ARCH=$$PLARCH PLBASE=$$PLBASE

PLSRC=	class.pl load.pl selector.pl types.pl \
	initquantity.pl methods.pl semantic.pl \
	interface.pl pllib.pl solve.pl \
	intern.pl reclass.pl terminal.pl

GARPDIRS=	models program_files program_files/$(ARCH)

all:		
		@echo "Usage:"
		@echo ""
		@echo "    make sunos          Make for SUNOS4"
		@echo "    make solaris        Make for SUNOS5 (solaris)"
		@echo "    make linux          Make for Linux/ELF"
		@echo ""
		@echo "    make static         Embedded executable (plld)" 
		@echo ""
		@echo "    make install        Install garp to $(garpdir)"

static:		
		plld -pl $(PL) $(COFLAGS) -goal go -toplevel halt -o garp pl-bit.c load.pl

sunos:		
		@eval `$(PL) -dump-runtime-variables` && \
		$(MAKE) $(PLVARS) LD=ld LDFLAGS="" DEFS=-DSUNOS4 bits dogarp
solaris:	
		eval `$(PL) -dump-runtime-variables` && \
		$(MAKE) $(PLVARS) bits dogarp
linux:		
		@eval `$(PL) -dump-runtime-variables` && \
		$(MAKE) $(PLVARS)  bits dogarp
install:
		@eval `$(PL) -dump-runtime-variables` && \
		$(MAKE) $(PLVARS) doinstall
garp:
		@eval `$(PL) -dump-runtime-variables` && \
		$(MAKE) $(PLVARS) dogarp

bits:		$(BITS)

pl-bit.o:	pl-bit.c

$(BITS):	pl-bit.o
		mkdir -p $(ARCH)
		$(LD) $(LDFLAGS) -o $@ pl-bit.o

dogarp:		$(PLSRC)
		$(PL) -F none -f none -g '[load],save_garp(garp),halt'

doinstall:	bits garp
		@for d in $(GARPDIRS); do \
		    mkdir -p $(garpdir)/$$d; \
		done
		cp $(PLSRC) $(garpdir)/program_files
		cp $(ARCH)/pl-bit.so $(garpdir)/program_files/$(ARCH)
		cp -r ../models/* $(garpdir)/models
		chmod -R go-w $(garpdir)
		chmod -R +r $(garpdir)
		cp garp $(bindir)
		chmod 755 $(bindir)/garp

clean:		
		rm -f *~ *% core a.out pl-bit.o

dist_clean:	clean
		rm -f garp
