## Beginner's Guide

In this guide it is my goal to introduce the reader to application development with Mation. This guide is minimal, covering *core* concepts but leaving additional information for later.

Unfortunately, I must admit that I don't think I succeeded in making this guide particularly accessible. It will be most easily grokked by readers who:

- Have used Elm
- Are comfortable with Purescript
- Are comfortable with lenses (`Setter`, `Lens`, composition) and `Proxy`

Partially this is due to my own inexperience writing technical documentation; partially this is because the Mation framework does *not* shy away from powerful-but-enigmatic tools (such as lenses. other fun things are also used internally, such as existential types and homomorphisms; we also came close to using recursion schemes!).
