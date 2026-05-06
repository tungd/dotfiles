SHELL := /bin/bash

APP_NAME ?= Emacs Host
EXECUTABLE := emacs-host
CONFIGURATION ?= release
CODESIGN_IDENTITY ?= $(or $(EMACS_HOST_CODESIGN_IDENTITY),-)
INSTALL_DIR ?= $(HOME)/Applications

BUILD_DIR := .build/$(CONFIGURATION)
BINARY := $(BUILD_DIR)/$(EXECUTABLE)
APP_DIR := dist/$(APP_NAME).app
INSTALLED_APP := $(INSTALL_DIR)/$(APP_NAME).app
CONTENTS_DIR := $(APP_DIR)/Contents
MACOS_DIR := $(CONTENTS_DIR)/MacOS
RESOURCES_DIR := $(CONTENTS_DIR)/Resources
INFO_PLIST := Resources/Info.plist

.PHONY: app build debug run install uninstall clean-app print-icon

app: build
	@set -euo pipefail; \
	icon_source="$${EMACS_HOST_ICON:-}"; \
	if [[ -z "$$icon_source" ]]; then \
	  emacs_path="$${EMACS_HOST_EMACS:-$$(command -v emacs || true)}"; \
	  if [[ -z "$$emacs_path" ]]; then \
	    echo "Could not find emacs. Set EMACS_HOST_EMACS or EMACS_HOST_ICON." >&2; \
	    exit 1; \
	  fi; \
	  resolved="$$emacs_path"; \
	  while [[ -L "$$resolved" ]]; do \
	    target="$$(readlink -- "$$resolved")"; \
	    if [[ "$$target" != /* ]]; then \
	      target="$$(dirname -- "$$resolved")/$$target"; \
	    fi; \
	    resolved="$$(cd -P -- "$$(dirname -- "$$target")" >/dev/null && printf '%s/%s\n' "$$(pwd -P)" "$$(basename -- "$$target")")"; \
	  done; \
	  probe="$$resolved"; \
	  bundle=""; \
	  while [[ "$$probe" != "/" && -n "$$probe" ]]; do \
	    if [[ "$$probe" == *.app ]]; then \
	      bundle="$$probe"; \
	      break; \
	    fi; \
	    probe="$$(dirname -- "$$probe")"; \
	  done; \
	  if [[ -z "$$bundle" ]]; then \
	    echo "Could not find an Emacs.app above $$resolved. Set EMACS_HOST_ICON." >&2; \
	    exit 1; \
	  fi; \
	  icon_name="$$("/usr/libexec/PlistBuddy" -c 'Print :CFBundleIconFile' "$$bundle/Contents/Info.plist" 2>/dev/null || true)"; \
	  if [[ -z "$$icon_name" ]]; then \
	    echo "Could not read CFBundleIconFile from $$bundle." >&2; \
	    exit 1; \
	  fi; \
	  if [[ "$$icon_name" != *.icns ]]; then \
	    icon_name="$$icon_name.icns"; \
	  fi; \
	  icon_source="$$bundle/Contents/Resources/$$icon_name"; \
	fi; \
	if [[ ! -f "$$icon_source" ]]; then \
	  echo "Icon not found: $$icon_source" >&2; \
	  exit 1; \
	fi; \
	if [[ ! -x "$(BINARY)" ]]; then \
	  echo "Built binary not found: $(BINARY)" >&2; \
	  exit 1; \
	fi; \
	mkdir -p "$(MACOS_DIR)" "$(RESOURCES_DIR)"; \
	cp -f "$(BINARY)" "$(MACOS_DIR)/$(EXECUTABLE)"; \
	cp -f "$(INFO_PLIST)" "$(CONTENTS_DIR)/Info.plist"; \
	cp -f "$$icon_source" "$(RESOURCES_DIR)/Emacs.icns"; \
	chmod 755 "$(MACOS_DIR)/$(EXECUTABLE)"; \
	plutil -lint "$(CONTENTS_DIR)/Info.plist" >/dev/null; \
	codesign --force --deep --sign "$(CODESIGN_IDENTITY)" "$(APP_DIR)"; \
	echo "$(APP_DIR)"

build:
	swift build -c $(CONFIGURATION)

debug:
	swift build

run:
	swift run $(EXECUTABLE)

install: app
	@if [[ -z "$(INSTALLED_APP)" || "$(INSTALLED_APP)" != *.app ]]; then \
	  echo "Refusing install path: $(INSTALLED_APP)" >&2; \
	  exit 1; \
	fi
	mkdir -p "$(INSTALL_DIR)"
	rm -rf "$(INSTALLED_APP)"
	ditto "$(APP_DIR)" "$(INSTALLED_APP)"
	@echo "$(INSTALLED_APP)"

uninstall:
	@if [[ -z "$(INSTALLED_APP)" || "$(INSTALLED_APP)" != *.app ]]; then \
	  echo "Refusing uninstall path: $(INSTALLED_APP)" >&2; \
	  exit 1; \
	fi
	rm -rf "$(INSTALLED_APP)"

print-icon:
	@$(MAKE) --no-print-directory app CONFIGURATION=$(CONFIGURATION) >/dev/null
	@/usr/libexec/PlistBuddy -c 'Print :CFBundleIconFile' "$(CONTENTS_DIR)/Info.plist"

clean-app:
	rm -rf dist
