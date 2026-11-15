FPM ?= fpm
PREFIX ?= $(HOME)/.local
# FortUI's terminal emulator; the desktop window needs no flag.
FLAGS = FPM_FFLAGS="$${FPM_FFLAGS:-} -O2 -cpp -DENABLE_TERM=1" \
	FPM_CFLAGS="$${FPM_CFLAGS:-} -O2 -DENABLE_TERM=1" \
	FPM_LDFLAGS="$${FPM_LDFLAGS:-} -lutil -lpthread"

.PHONY: all build run test install clean

all: build

build:
	$(FLAGS) $(FPM) build

run:
	$(FLAGS) $(FPM) run -- $(ARGS)

test:
	$(FLAGS) $(FPM) test

# The program, its desktop entry, and the XFCE helper through which XFCE can
# choose it as the terminal emulator (Default Applications > Utilities).
install:
	$(FLAGS) $(FPM) install --prefix "$(CURDIR)/build/stage"
	# A new file, not an overwrite: running windows keep their program.
	install -Dm755 build/stage/bin/fortterm "$(PREFIX)/bin/fortterm"
	install -Dm644 data/fortterm.desktop "$(PREFIX)/share/applications/fortterm.desktop"
	sed -i 's|^Exec=.*|Exec=$(PREFIX)/bin/fortterm|' \
		"$(PREFIX)/share/applications/fortterm.desktop"
	install -Dm644 data/fortterm-helper.desktop \
		"$(PREFIX)/share/xfce4/helpers/fortterm.desktop"
	sed -i 's|^X-XFCE-Binaries=.*|X-XFCE-Binaries=$(PREFIX)/bin/fortterm;fortterm;|' \
		"$(PREFIX)/share/xfce4/helpers/fortterm.desktop"
	-update-desktop-database "$(PREFIX)/share/applications"

clean:
	$(FPM) clean --skip
