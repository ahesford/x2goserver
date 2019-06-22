#!/usr/bin/make -f

SRC_DIR=$(CURDIR)
SHELL=/bin/bash

INSTALL_DIR=install -d -o root -g root -m 755
INSTALL_FILE=install -o root -g root -m 644
INSTALL_PROGRAM=install -o root -g root -m 755

RM_FILE=rm -f
RM_DIR=rmdir -p --ignore-fail-on-non-empty

DESTDIR ?=
PREFIX ?= /usr/local
ETCDIR ?= /etc/x2go
BINDIR ?= $(PREFIX)/bin
SBINDIR ?= $(PREFIX)/sbin
LIBDIR ?= $(PREFIX)/lib/x2go
MANDIR ?= $(PREFIX)/share/man
SHAREDIR ?= $(PREFIX)/share/x2go

BIN_SCRIPTS=$(shell cd bin && echo *)
SBIN_SCRIPTS=$(shell cd sbin && echo *)
LIB_FILES=$(shell cd lib && echo *)
FEATURE_SCRIPTS=$(shell cd share/x2gofeature.d && echo *.features)
POD_SCRIPTS=sbin/x2gocleansessions.8 bin/x2goupdateoptionsstring.1

man_pages = `cd man && find * -type f`

MAN2HTML_BIN  = $(shell which man2html)
MAN2HTML_SRC  = man
MAN2HTML_DEST = .build_man2html/html

all: clean build

build: build-arch build-indep

build-arch:

build-indep: build_pod2man build_man2html
	for dir in 'bin' 'sbin' 'lib' 'share/x2gofeature.d'; do \
		for gen_file in "$${!dir}/"*; do \
			file="$${gen_file%.in}"; \
			if [ "$${gen_file}" != "$${file}" ]; then \
				$$("# Copy the file first, making sure we keep permissions."); \
				cp -a "$${gen_file}" "$${file}"; \
				\
				$$("# Run actual substitutions."); \
				PREFIX='$(PREFIX)'; \
				ETCDIR='$(ETCDIR)'; \
				BINDIR='$(BINDIR)'; \
				SBINDIR='$(SBINDIR)'; \
				LIBDIR='$(LIBDIR)'; \
				MANDIR='$(MANDIR)'; \
				SHAREDIR='$(SHAREDIR)'; \
				for subst in 'PREFIX' 'ETCDIR' 'BINDIR' 'SBINDIR' 'LIBDIR' 'MANDIR' 'SHAREDIR'; do \
					sed -e "s#@@$${subst}@@#$${!subst}#g" "$${gen_file}" > "$${file}"; \
				done; \
			fi; \
		done; \
	done

build_pod2man:
	for sectioned_script in $(POD_SCRIPTS); do \
		man_page="$$(basename "$${sectioned_script}")"; \
		section="$${man_page##*.}"; \
		script="$${sectioned_script%.*}"; \
		$(INSTALL_DIR) "man/man$${section}"; \
		case "$${suite}" in \
			('1') \
				suite='General Commands Manual'; \
				;; \
			('5') \
				suite='X2Go Server Configuration'; \
				;; \
			('8') \
				suite='X2Go Server Tool'; \
				;; \
			(*) \
				suite='Unknown - X2Go Server Bug!'; \
				;; \
		esac; \
		pod2man -s "$${section}" -c "$${suite}" -r "Version $$(cat 'VERSION.x2goserver')" "$${script}" > "man/man$${section}/$${man_page}"; \
	done

build_man2html:
	if [ -n "$(MAN2HTML_BIN)" ]; then \
	    mkdir -p $(MAN2HTML_DEST); \
	    for man_page in $(man_pages); do mkdir -p `dirname $(MAN2HTML_DEST)/$$man_page`; done; \
	    for man_page in $(man_pages); do $(MAN2HTML_BIN) $(MAN2HTML_SRC)/$$man_page > $(MAN2HTML_DEST)/$$man_page.html; done; \
	fi

clean: clean-arch clean-indep

clean-arch:

clean-indep: clean_man2html
	for dir in 'bin' 'sbin' 'lib' 'share/x2gofeature.d'; do \
		for gen_file in "$${!dir}/"*; do \
			file="$${gen_file%.in}"; \
			if [ "$${gen_file}" != "$${file}" ]; then \
				$(RM_FILE) "$${file}"; \
			fi; \
		done; \
	done

clean_man2html:
	rm -Rf `dirname $(MAN2HTML_DEST)`

install: build install_scripts install_config install_man install_version

install_scripts:
	for dir in 'bin' 'sbin' 'lib' 'share/x2gofeature.d'; do \
		$("# Create (and save) destination directory."); \
		dest='$(DESTDIR)'; \
		case "$${dir}" in \
			('bin') dest="$${dest}$(BINDIR)/";; \
			('sbin') dest="$${dest}$(SBINDIR)/";; \
			('lib') dest="$${dest}$(LIBDIR)/";; \
			('share/x2gofeature.d') dest="$${dest}$(SHAREDIR)/x2gofeature.d/";; \
			(*) printf '%s\n' 'Unknown component in install_scripts scriptlet.' >&2; exit '1';; \
		esac; \
		$(INSTALL_DIR) "$${dest}"; \
		\
		$("# Iterate over files and ..."); \
		for gen_file in "$${!dir}/"*; do \
			file="$${gen_file%.in}"; \
			$("# ... filter out generation templates and anything not ending in '.features' for the feature directory."); \
			if [ "$${gen_file}" != "$${file}" ] || ( [ "$${dir}" = 'share/x2gofeature.d' ] && [ "$${file%.features}" = "$${file}" ] ); then \
				continue; \
			fi; \
			\
			$(INSTALL_PROGRAM) "$${file}" "$${dest}/"; \
		done; \
	done

install_config:
	$(INSTALL_DIR) $(DESTDIR)$(ETCDIR)
	$(INSTALL_DIR) $(DESTDIR)$(ETCDIR)/x2go_logout.d
	$(INSTALL_FILE) etc/x2go_logout       $(DESTDIR)$(ETCDIR)/
	$(INSTALL_FILE) etc/x2go_logout.d/*   $(DESTDIR)$(ETCDIR)/x2go_logout.d/
	$(INSTALL_DIR) $(DESTDIR)/etc/sudoers.d || true
	$(INSTALL_FILE) etc/sudoers.d/x2goserver $(DESTDIR)/etc/sudoers.d/
	$(INSTALL_DIR) $(DESTDIR)/etc/logcheck/ignore.d.server || true
	$(INSTALL_FILE) etc/logcheck/ignore.d.server/x2goserver $(DESTDIR)/etc/logcheck/ignore.d.server/

install_man:
	$(INSTALL_DIR) $(DESTDIR)$(MANDIR)
	$(INSTALL_DIR) $(DESTDIR)$(MANDIR)/man1
	$(INSTALL_FILE) man/man1/*.1           $(DESTDIR)$(MANDIR)/man1
	gzip -f $(DESTDIR)$(MANDIR)/man1/x2go*.1
	$(INSTALL_DIR) $(DESTDIR)$(MANDIR)/man5
	$(INSTALL_FILE) man/man5/*.5           $(DESTDIR)$(MANDIR)/man5
	gzip -f $(DESTDIR)$(MANDIR)/man5/x2go*.5
	$(INSTALL_DIR) $(DESTDIR)$(MANDIR)/man8
	$(INSTALL_FILE) man/man8/*.8           $(DESTDIR)$(MANDIR)/man8
	gzip -f $(DESTDIR)$(MANDIR)/man8/x2go*.8

install_version:
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)
	$(INSTALL_DIR) $(DESTDIR)$(SHAREDIR)/versions
	$(INSTALL_FILE) VERSION.x2goserver     $(DESTDIR)$(SHAREDIR)/versions/VERSION.x2goserver

uninstall: uninstall_scripts uninstall_config uninstall_man uninstall_version

uninstall_scripts:
	for dir in 'bin' 'sbin' 'lib' 'share/x2gofeature.d'; do \
		$("# Create (and save) destination directory."); \
		dest='$(DESTDIR)'; \
		case "$${dir}" in \
			('bin') dest="$${dest}$(BINDIR)/";; \
			('sbin') dest="$${dest}$(SBINDIR)/";; \
			('lib') dest="$${dest}$(LIBDIR)/";; \
			('share/x2gofeature.d') dest="$${dest}$(SHAREDIR)/x2gofeature.d/";; \
			(*) printf '%s\n' 'Unknown component in uninstall_scripts scriptlet.'; exit '1';; \
		esac; \
		$(INSTALL_DIR) "$${dest}"; \
		\
		$("# Iterate over files and ..."); \
		for gen_file in "$${!dir}/"*; do \
			$("# ... map generation templates to generated files (so that uninstallation works correctly after cleaning as well)."); \
			file="$${gen_file%.in}"; \
			$(RM_FILE) "$${dest}/$${file}"; \
		done; \
	done
	$(RM_DIR) $(DESTDIR)$(LIBDIR) || true
	$(RM_DIR) $(DESTDIR)$(SHAREDIR)/x2gofeature.d || true
	$(RM_DIR) $(DESTDIR)$(SHAREDIR) || true

uninstall_config:
	$(RM_FILE) $(DESTDIR)$(ETCDIR)/x2go_logout.d/*
	$(RM_FILE) $(DESTDIR)$(ETCDIR)/x2go_logout
	$(RM_DIR)  $(DESTDIR)$(ETCDIR)/x2go_logout.d || true
	$(RM_DIR)  $(DESTDIR)$(ETCDIR) || true
	$(RM_FILE) $(DESTDIR)/etc/sudoers.d/x2goserver
	$(RM_DIR)  $(DESTDIR)/etc/sudoers.d || true
	$(RM_FILE) $(DESTDIR)/etc/logcheck/ignore.d.server/x2goserver
	$(RM_DIR)  $(DESTDIR)/etc/logcheck/ignore.d.server || true
	$(RM_DIR)  $(DESTDIR)/etc/logcheck || true

uninstall_man:
	for file in $(BIN_SCRIPTS); do $(RM_FILE) $(DESTDIR)$(MANDIR)/man8/$$file.8.gz; done
	for file in $(SBIN_SCRIPTS); do $(RM_FILE) $(DESTDIR)$(MANDIR)/man8/$$file.8.gz; done
	$(RM_DIR)  $(DESTDIR)$(MANDIR) || true

uninstall_version:
	$(RM_FILE) $(DESTDIR)$(SHAREDIR)/versions/VERSION.x2goserver
	$(RM_DIR)  $(DESTDIR)$(SHAREDIR)/versions || true
