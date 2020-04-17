################################################################
# Build the GARP pl-bit.dll
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=e:\jan\installed\pl
PLLIB=$(PLHOME)\lib\libpl.lib
PLTERM=$(PLHOME)\lib\plterm.lib
PKGDLL=pl-bit
BINDIR=i386-win32
LD=link.exe
LDFLAGS=/export:init_bits

INCLUDE=$(PLHOME)\include;$(INCLUDE)

OBJ=		pl-bit.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(PLTERM)

install::
		copy $(PKGDLL).dll $(BINDIR)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

