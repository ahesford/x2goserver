#!/usr/bin/make -f

SRC_DIR=$(CURDIR)
SHELL=/bin/bash

INSTALL_DIR=install -d -o root -g root -m 755
INSTALL_FILE=install -o root -g root -m 644
INSTALL_PROGRAM=install -o root -g root -m 755
INSTALL_SYMLINK=ln -s -f

RM_FILE=rm -f
RM_DIR=rmdir -p --ignore-fail-on-non-empty

DESTDIR ?=
PREFIX ?= /usr/local
ETCDIR=/etc/x2go
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/x2go
NXLIBDIR ?= $(LIBDIR)/../nx
MANDIR=$(PREFIX)/share/man
SHAREDIR=$(PREFIX)/share/x2go

FEATURE_SCRIPTS=$(shell cd share/x2go/x2gofeature.d && echo *.features)

man_pages = `cd man && find * -type f`

MAN2HTML_BIN  = $(shell which man2html)
MAN2HTML_SRC  = man
MAN2HTML_DEST = .build_man2html/html

all: clean build

build: build-arch build-indep

build-arch:

build-indep: build_man2html

build_man2html:

clean: clean_man2html

clean_man2html:

install: install_scripts install_config install_data install_man install_version

install_scripts:
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)/x2gofeature.d
	$(INSTALL_PROGRAM) share/x2go/x2gofeature.d/*.features $(DESTDIR)$(SHAREDIR)/x2gofeature.d/

install_data:

install_config:
	$(INSTALL_DIR) $(DESTDIR)$(ETCDIR)/
	$(INSTALL_FILE) etc/x2gokdrive.options $(DESTDIR)$(ETCDIR)/

install_man:

install_version:
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)/versions
	$(INSTALL_FILE) VERSION.x2goserver-x2gokdrive $(DESTDIR)$(SHAREDIR)/versions/

uninstall: uninstall_scripts uninstall_config uninstall_data uninstall_man uninstall_version

uninstall_scripts:
	for file in $(FEATURE_SCRIPTS); do $(RM_FILE) $(DESTDIR)$(SHAREDIR)/x2gofeature.d/$$file; done

uninstall_data:

uninstall_config:
	$(RM_FILE) $(DESTDIR)$(ETCDIR)/x2gokdrive.options
	$(RM_DIR)  $(DESTDIR)$(ETCDIR) || true

uninstall_man:

uninstall_version:
	$(RM_FILE) $(DESTDIR)$(SHAREDIR)/versions/VERSION.x2goserver-x2gokdrive
	$(RM_DIR)  $(DESTDIR)$(SHAREDIR)/versions || true
