# fortterm

A terminal emulator written in Fortran on [FortUI](https://github.com/lazy-fortran/fortui).
It shows images from the kitty graphics protocol and from sixel strings, so
tools such as `chafa`, `kitten icat`, tmux (with sixel) and FortUI's own
terminal applications draw real pixels. It runs in a window on the GPU (SDL3)
or inside another terminal, and it takes the command line of xfce4-terminal, so
XFCE can use it as its terminal emulator.

```sh
make            # needs SDL3, SDL3_ttf, fontconfig and libvterm
make install    # ~/.local/bin/fortterm, desktop entry, XFCE helper
fortterm                         # the login shell
fortterm -x htop                 # a program with its arguments
fortterm -e "ls -l | less" -H    # a command line; keep the window open
fortterm --working-directory=DIR --font-size=12 --colors=light
```

Keys: Ctrl+Shift+C copies the selection, Ctrl+Shift+V pastes, Ctrl+Shift+T
opens a tab, Ctrl+Shift+W closes it, Ctrl+Shift+Left/Right switch tabs,
Ctrl+Shift+= and Ctrl+Shift+- change the text size, the wheel and Shift+Page
Up/Down move through the scrollback. The window takes the title the program
sets.

To make it XFCE's terminal, choose fortterm under Settings > Default
Applications > Utilities > Terminal Emulator after `make install`.

The screen state comes from libvterm (MIT); the images, the drawing and
everything else are FortUI. The text face is the monospace face of the FortUI
theme (`FORTUI_THEME`, or `theme =` in `~/.config/fortui/config`).

## Dependencies and licences

fortterm is MIT licensed ([LICENSE](LICENSE)).

| Needs | Licence |
|---|---|
| FortUI (fetched by fpm) | MIT |
| SDL3, SDL3_ttf, fontconfig | zlib; zlib; MIT-style |
| libvterm 0.3+ | MIT |
