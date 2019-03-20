# haskell-format-import

This is a neovim plugin that formats your Haskell import statements, and it is itself written in Haskell!

How :fire: is that?!

![](haskell-format-import.gif)

## Install

- You will need to install [nvim-hs](https://github.com/neovimhaskell/nvim-hs).
- Then copy `HaskellFormatImport` into `~/.config/nvim/nvim-hs/HaskellFormatImport.hs`
- And copy `HaskellFormatImport/Plugin.hs` into `~/.config/nvim/nvim-hs/HaskellFormatImport/Plugin.hs`

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

