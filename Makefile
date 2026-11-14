FPM ?= fpm
PREFIX ?= $(HOME)/.local
# FortUI's SDL3 GPU backend and its terminal emulator (libvterm).
FLAGS = FPM_FFLAGS="$${FPM_FFLAGS:-} -O2 -cpp -DENABLE_SDL=1 -DENABLE_TERM=1" \
	FPM_CFLAGS="$${FPM_CFLAGS:-} -O2 -DENABLE_TERM=1" \
	FPM_LDFLAGS="$${FPM_LDFLAGS:-} -lSDL3 -lSDL3_ttf -lfontconfig -lvterm -lutil -lpthread"

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
	$(FLAGS) $(FPM) install --prefix "$(PREFIX)"
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
