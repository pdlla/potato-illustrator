# tinytools
`tinytools` is a mono-space unicode flow-chart editor written in Haskell.

# architecture
`tinytools` follows a strict MVC architecture. This repository contains the platform-independent model and controller.
The view/controller is connected to the model using a [reflex](https://github.com/reflex-frp/reflex) interface defined by `GoatWidget`.

[tinytools-vty](https://github.com/pdlla/tinytools-vty) is currently the only view implementation. It is written in [reflex-vty](https://github.com/reflex-frp/reflex-vty) and runs in a terminal yay. Please see [tinytools-vty](https://github.com/pdlla/tinytools-vty) if you'd like to try it out.

# features (completed)
- sophisticated hierarchical layer system
- transactional operations and change history
- several highly configurable primitives including boxes, lines and text boxes
- basic document save/load/export functionality

# roadmap

## v1
- UNICODE wide character support (currently blocked by issues in TextZipper)
- UNICODE glyph widget

## v2
- multi-document support
- refactor handle non-linear action do/undo operations in preparation for multi-user mode

## v3
- grapheme clusters support (blocked by terminal support which is currently extremely inconsistent or non-existent)
- multi-user mode
  - ordering service interface
  - basic single client authoritative implementation of ordering service interface (for now)
- scriptable command interface
