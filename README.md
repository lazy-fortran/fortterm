# fortterm

A terminal emulator written in Fortran on [FortUI](https://github.com/lazy-fortran/fortui).
It shows images from the kitty graphics protocol and from sixel strings, so
tools such as `chafa`, `kitten icat`, tmux (with sixel) and FortUI's own
terminal applications draw real pixels. It runs in a window on the GPU
(Vulkan) or inside another terminal, and it takes the command line of
xfce4-terminal, so XFCE can use it as its terminal emulator.

```sh
make            # needs only gfortran; at run time X11 (libxcb) and a Vulkan driver
make install    # ~/.local/bin/fortterm, desktop entry, XFCE helper
fortterm                         # the login shell
fortterm -x htop                 # a program with its arguments
fortterm -e "ls -l | less" -H    # a command line; keep the window open
fortterm --working-directory=DIR --font-size=12 --colors=light --maximize
```

Keys: Ctrl+Shift+C copies the selection, Ctrl+Shift+V pastes, Ctrl+Shift+T
opens a tab, Ctrl+Shift+W closes it, Ctrl+Shift+Left/Right switch tabs,
Ctrl+Shift+= and Ctrl+Shift+- change the text size, the wheel and Shift+Page
Up/Down move through the scrollback. The window takes the title the program
sets. URLs are underlined under the pointer and Ctrl+click opens them, also
inside tmux. Selecting copies to the primary selection (middle-click pastes);
Shift selects even when a program takes the mouse; programs such as tmux can set
the clipboard (OSC 52), but not read it.

To make it XFCE's terminal, choose fortterm under Settings > Default
Applications > Utilities > Terminal Emulator after `make install`.

fortterm takes font, colours, cursor and window size from your xfce4-terminal
settings (xfconf, else `terminalrc`), and draws the text as xfce4-terminal does,
pixel for pixel; command-line options override them.

Startup is instant: after a start, one fortterm waits hidden with its window and
GPU ready (asleep, no CPU), and the next `fortterm` hands it the command line,
directory and environment over a socket in `$XDG_RUNTIME_DIR`; its window
appears within about 50 ms. A spare left from an older build steps aside.
`FORTTERM_TIMING=1` prints where the time goes.

Everything is Fortran on FortUI: the terminal engine, the images and the
drawing; it shows 400 MB of coloured text in about 3 s, as fast as `cat` and
the kernel deliver it.

## Dependencies and licences

fortterm is MIT licensed ([LICENSE](LICENSE)).

| Needs | Licence |
|---|---|
| FortUI (fetched by fpm) | MIT |
| at run time, opened rather than linked: libxcb, the Vulkan loader and a Vulkan 1.3 driver | MIT; Apache-2.0; the driver's |
