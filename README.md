# haskell-format-import

[![Build Status](https://travis-ci.com/chris-bacon/haskell-format-import.svg?branch=master)](https://travis-ci.com/exosphere-lang/compiler)

This is a neovim plugin that formats your Haskell import statements, and *it is itself written in Haskell!*

How :fire: is that?!

![](haskell-format-import.gif)

## Features

- Sorts alphabetically
- Aligns on "qualified"
- Aligns on "as"
- Aligns on "()"

## Install

- You will need to install [nvim-hs](https://github.com/neovimhaskell/nvim-hs).
- Then copy `src/HaskellFormatImport` to `~/.config/nvim/nvim-hs/HaskellFormatImport.hs`
- And copy `src/HaskellFormatImport/Plugin.hs` to `~/.config/nvim/nvim-hs/HaskellFormatImport/Plugin.hs`

Then add the following code to your `~/.config/nvim/nvim.hs` file,

```Haskell
..

import qualified HaskellFormatImport as HFI

..

main = do
  neovim defaultConfig
    { plugins = plugins defaultConfig ++ [ .., HFI.plugin ]
    }
```

## Usage

This plugin exposes the `HaskellFormatImport` command which you could map some keybinding to, or invoke directly.

## Tests

To run the tests you can run `stack test`
