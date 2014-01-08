# .emacs.d

My Emacs configuration tree, continually used and cooked since 2012.
It has a lot of snippets collected around the web and might be a
valuable source of tips.

*Requires Emacs 24.3 or greater*.

## Files

All the files are inside `~/.emacs.d`. They're listed in order of
loading.

| file                                | contents |
| ----------------------------------- | -------- |
| `init.el`                           | First file to be read by Emacs, where it all starts. Loads all the other files. |
| `config/*`                          | Where every other config file is. |
| `config/elpa.el`                    | Starting the Emacs Package Manager |
| `elpa/*`                            | Packages installed by the Emacs package manager. |
| `config/functions.el`               | Random functions I couldn't fit elsewhere - comment and duplicate line, new C++ class, etc. |
| `config/general.el`                 | Misc. stuff, like disabling auto-save, kill ring size, bookmark folder... |
| `config/editing.el`                 | Things related to manipulting text - shortcuts, delete trailing spaces... |
| `config/modes.el`                   | Starting modes and binding them to file extensions. |
| `config/macros.el`                  | My saved recorded macros. |
| `config/interface.el`               | Customizing the editor's appearance. |
| `config/alexdantas-color-theme.el`  | My custom color theme. Very nice, I assure you ;) |
| `config/ide.el`                     | A (failed) attempt to use CEDET, thus turning Emacs into a C/C++ IDE. |
| `config/keybindings.el`             | All global keybindings centered here. |
| `local`                             | Local .el files that I couldn't find on package managers. |

## Inspiring Links

Here's some links to other sources of Emacs configs, tips
and tricks:

* [`purcell/emacs.d`](https://github.com/purcell/emacs.d)
* [`overtone/emacs-live`](https://github.com/overtone/emacs-live)
* [`defunkt/emacs`](https://github.com/defunkt/emacs)
* [`cjohansen/.emacs.d`](https://github.com/cjohansen/.emacs.d)
* [`alexott/emacs-configs`](https://github.com/alexott/emacs-configs)
* [`ghoseb/dotemacs`](https://github.com/ghoseb/dotemacs)

