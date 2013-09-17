IDE Integrations
================

Sublime Text 2
--------------

We provide a configuration for SublimeREPL for running ClaferIG inside Sublime as a REPL.

### Usage

Use `<CTRL>+i`, `g`, `s` to open a Clafer `.cfr` file with ClaferIG and `--ss=simple` setting.
Use `<CTRL>+i`, `g`, `f` to open a Clafer `.cfr` file with ClaferIG and `--ss=full` setting.
This commands assume that Clafer and ClaferIG executables are in system `PATH`.

### Installation 

1. Install `SublimeREPL` plugin.
  * You need to install `Package Control` first - see [installation](http://wbond.net/sublime_packages/package_control/installation). To open the Console, go to `View->Show Console`.
  * Go to `Preferences->Package Control`
  * type `Install`, `<Enter>`
  * type `SublimeREPL`, `<Enter>`
2. Copy the folder `ClaferIG` from `IDEs\sublime-text-2\Packages\SublimeREPL\config\` to `Packages\SublimeREPL\config\` in Sublime Text 2 folder inside your home directory.
3. In Sublime, open `Preferences->Key Bindings - User` and add the following key bindings there (inside the `[ ]`, remember about commas `,` to have a proper list)

```
    {
        "keys": [ "ctrl+i", "g", "s" ],
        "args": {
            "id": "repl_claferIG",
            "file": "config/ClaferIG/IGS.sublime-menu"
        },
       "command": "run_existing_window_command"
    },
    {
        "keys": [ "ctrl+i", "g", "f" ],
        "args": {
            "id": "repl_claferIG",
            "file": "config/ClaferIG/IGF.sublime-menu"
        },
       "command": "run_existing_window_command"
    }
```

> Note: 
> on Windows 7 and 8, the folder is `<user name>\AppData\Roaming\Sublime Text 2\Packages`.
> on Mac, the folder is `~/Library/Application Support/Sublime Text 2/Packages`.