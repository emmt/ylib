# where are the sources? (automatically filled in by configure script)
srcdir=.

# these values filled in by "yorick -batch make.i" or configure script
Y_MAKEDIR=
Y_EXE=
Y_EXE_PKGS=
Y_EXE_HOME=
Y_EXE_SITE=
Y_HOME_PKG=

# ------------------------------------------------ macros for this package

PKG_NAME=ylib
PKG_I=${srcdir}/fft_utils.i \
      ${srcdir}/fmin.i \
      ${srcdir}/img.i \
      ${srcdir}/linalg.i \
      ${srcdir}/mda.i \
      ${srcdir}/options.i \
      ${srcdir}/plot.i \
      ${srcdir}/rgb1.i \
      ${srcdir}/rgb2.i \
      ${srcdir}/rgb3.i \
      ${srcdir}/rgb.i \
      ${srcdir}/utils.i \
      ${srcdir}/xplot0.i \
      ${srcdir}/xplot.i \
      ${srcdir}/xsort.i

# autoload file for this package, if any
PKG_I_START= ${srcdir}/ylib-start.i

# non-pkg.i include files for this package, if any
PKG_I_EXTRA=

RELEASE_FILES = AUTHORS LICENSE Makefile NEWS README.md \
	configure ${PKG_I} ${PKG_I_START}
RELEASE_NAME = $(PKG_NAME)-$(RELEASE_VERSION).tar.bz2

# --------------------------------------- standard macros for all packages

# installation directories
DESTDIR=
DEST_Y_SITE=$(DESTDIR)$(Y_EXE_SITE)
DEST_Y_HOME=$(DESTDIR)$(Y_EXE_HOME)
DEST_Y_BINDIR=$(DESTDIR)$(Y_BINDIR)

# installation commands
Y_LIBEXE=$(Y_EXE_HOME)/lib
Y_GROUP=`cat $(Y_LIBEXE)/install.grp`
YNSTALL=$(Y_LIBEXE)/install.sh $(Y_GROUP)

# -------------------------------------------- standard targets and rules

default:
	@echo "nothing to do, type 'make install' to install"

install:
	@if test -n "$(PKG_I)" -o -n "$(PKG_I_EXTRA)"; then \
	  for file in $(PKG_I) $(PKG_I_EXTRA); do \
	    echo "Installing $$file in Y_HOME/i"; \
	    $(YNSTALL) "$$file" $(DEST_Y_HOME)/i; \
	  done; \
	fi; \
	if test -n "$(PKG_I_START)"; then \
	  for file in $(PKG_I_START); do \
	    echo "Installing $$file in Y_HOME/i-start"; \
	    $(YNSTALL) "$$file" $(DEST_Y_HOME)/i-start; \
	  done; \
	fi

uninstall:
	@if test -n "$(PKG_I)" -o -n "$(PKG_I_EXTRA)"; then \
	  for file in $(PKG_I) $(PKG_I_EXTRA); do \
	    base=`basename "$$file"`; \
	    full="$(DEST_Y_HOME)/i/$$base"; \
	    if test -f "$$full"; then \
	      echo "Uninstalling $$base from Y_HOME/i"; \
	      $(RM) "$$full"; \
	    fi; \
	  done; \
	fi; \
	if test -n "$(PKG_I_START)"; then \
	  for file in $(PKG_I_START); do \
	    base=`basename "$$file"`; \
	    full="$(DEST_Y_HOME)/i-start/$$base"; \
	    if test -f "$$full"; then \
	      echo "Uninstalling $$base from Y_HOME/i-start"; \
	      $(RM) "$$full"; \
	    fi; \
	  done; \
	fi

clean:
	rm -f *~ core* *.core

ylib-start-tmp.i: ${PKG_I}
	rm -f $@
	for x in ${PKG_I}; do \
	  echo -n "autoload, \"$$x\", " >> $@; \
	  grep <$$x -E '^[ 	]*func[ 	]+[^_]' \
	  | sed -e 's/^[ 	]*func[ 	][ 	]*\([_0-9A-Za-z]*\).*/\1/' >> $@; \
	done


release: $(RELEASE_NAME)

$(RELEASE_NAME):
	@if test "x$(RELEASE_VERSION)" = "x"; then \
	  echo >&2 "set package version:  make RELEASE_VERSION=... release"; \
	else \
          dir=`basename "$(RELEASE_NAME)" .tar.bz2`; \
	  if test "x$$dir" = "x" -o "x$$dir" = "x."; then \
	    echo >&2 "bad directory name for archive"; \
	  elif test -d "$$dir"; then \
	    echo >&2 "directory $$dir already exists"; \
	  else \
	    mkdir -p "$$dir"; \
	    for file in $(RELEASE_FILES); do \
	      file=`basename "$$file"`; \
	      src="$(srcdir)/$$file"; \
	      dst="$$dir/$$file"; \
	      if test "$$file" = "Makefile"; then \
	        sed <"$$src" >"$$dst" -e 's/^\( *Y_\(MAKEDIR\|EXE\(\|_PKGS\|_HOME\|_SITE\)\|HOME_PKG\) *=\).*/\1/'; \
	        touch -r "$$src" "$$dst"; \
	      else \
	        cp -p "$$src" "$$dst"; \
	      fi; \
	    done; \
	    rm -f "$$dir"/*~ "$$dir"/*/*~; \
	    echo "$(RELEASE_VERSION)" > "$$dir/VERSION"; \
	    tar jcf "$(RELEASE_NAME)" "$$dir"; \
	    rm -rf "$$dir"; \
	    echo "$(RELEASE_NAME) created"; \
	  fi; \
	fi;

.PHONY: clean release

# -------------------------------------------------------- end of Makefile
