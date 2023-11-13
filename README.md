# DDevExtensions
Homepage: https://www.idefixpack.de/ddev

DDevExtensions adds new features to RAD Studio.

Supported Delphi Versions 10.2, 10.3, 10.4, 11.x, 12.0 

## Releases Delphi 2009-10.4

Until there are new releases the releases are still available at
https://www.idefixpack.de/ddev

## Release Delphi 11.x, 12.0

A precompiled version can be downloaded from the DelphiPraxis fork:
https://github.com/DelphiPraxis/DDevExtensions/releases


## Compile

Requires: jedi\jedi.inc file (from https://github.com/project-jedi/jedi) in the IDE's source path.

In the `Code\DDevExtensions folder` open the `DDevExtensions.groupproj` file in the Delphi IDE.
The file is in the D_D102 (Delphi 10.2), D_D103 (Delphi 10.3), D_D104 (Delphi 10.4), ... folder.


## How to install

Simply start the DDevExtensionsReg.exe.

This will copy files to $(APPDATA)\DDevExtensions and it registers the expert DLLs
in the registry.


## How to uninstall

Start the InstallDDevExtensions.exe and press the <Uninstall> button.


## Features

- Disable Package Cache (default: off)
- Disable Source Formatter hotkey (default: off)
- Show project for active file in Project Manager (default: on)
- Editor tab double click action (default: zoom)
- Structure View Search (default: no hotkey)
- Increment Build Number only when building the project (default: on) [2010 only]
- Set TLabel.Margins.Bottom to zero (default: on)
- Remove Explicit* properties (default: off)
- Component Selector (default: off, no hotkey)
- Disable “Source has been modified. Rebuild?” (default: on)
- Auto-save editor files after successful compile (default: off)
- Switch project to current file’s project (default: on)
- Put “Last Compile” time into version info (default: off) [2009-XE]
- Find Unit/Use Unit replacement dialog (default: on)
- File Cleaner (default: on)
- Compile Backup (default: on)
- Loading package tweaks
- Enhanced Key Bindings (default: on)
- Old Component Palette (default: off) [2009-10.2]
- Set Project Versioninfo dialog
- Compile Progress improvements (progressbar, taskbar progress)
- Replace “Package Add Unit” dialog with “File Open” dialog [2009 only]
- Close and Kill the IDE
- Replace Open File At Cursor (default: off)
- Show all inheritable modules (default: off)
- Don’t break when starting spawned processes (default: off)
- Disable Code folding (default: off) [2009 only]
- Kill all dexplore.exe when closing the IDE (default: on)
- Show confirmation dialog for Ctrl+F1 while debugging (default: on)
- Release compiler unit cache before compiling (default: off)
- Improved reload changed files dialog
- Local Start Parameters (default: off) [2009-XE]
- Project Start Parameters (default: off)
