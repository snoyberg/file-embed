# file-embed ![Hackage](https://img.shields.io/hackage/v/file-embed.svg) ![Hackage-Deps](https://img.shields.io/hackage-deps/v/file-embed.svg) ![Tests](https://github.com/snoyberg/file-embed/workflows/Tests/badge.svg)

Embed arbitrary files like JSON or plain text into your Haskell code as if they were written inside Haskell.

Note: This library uses Template Haskell for the embedding.

## Quickstart

Add [the latest version](https://hackage.haskell.org/package/file-embed) of `file-embed` to your
package description `<package>.cabal` or Stack `package.yaml` file.

Given the folder structure

```shell
$ tree
.
└── myapp
│   ├── app
│   │   └── Main.hs
│   ├── embedded.json
│   └── myapp.cabal
└── cabal.project
```

you can embed a file as follows:

```haskell
-- file: Main.hs
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFileRelative)

myFile :: BS.ByteString
myFile = $(embedFileRelative "embedded.json")
```

The path to `embedFileRelative` is relative to the package root; the folder where the `<package>.cabal` file is.
Take a look at the [Hackage documentation](https://hackage.haskell.org/package/file-embed/docs/Data-FileEmbed.html)
for more examples and variations.
