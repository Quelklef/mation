# Mation

- **Mation = Elm in Purescript + ergonomic IO + simple components**

- Mation is in development.


## Features

- Elmlike: familiar `Model`/`view` architecture,

- Simple components: a component is written exactly the same as an app. You create a `ComponentModel` type and a `renderComponent` view, and you're done. No lifecycle hooks, no nothing. Simple.

- Monolithic state: even with one components, your application has all its state in one place. Whereas in React or Vue a child component may take on state unknown to the parent component, the same is not possible in Mation.

- Ergonomic IO: as event handlers become more complex, the code does not. Want to run a pure state update on button press? Sure. Need to perform a monadic effect as well? You got it. Need to perform *multiple*? Not a problem! Need to execute a long-running process, occasionally updating the model of your progress, and finally concluding? Still covered.

- Monoidal HTML: monoidal affordances like `foldMap` can be used to create `Html` values, which notably improves ergonomics. No more awkward appending a list of `Html` values to another list!

- Powerful styling: give an element a `background-color` with `S.backgroundColor "red"`. Do it on `:hover` with `S.on Sel.hover [ S.backgroundColor "red" ]`. Do it on children, but only when hovered and `@media print` holds, with `S.on (Sel.hover >> Sel.children >> Sel.media "print") [ S.backgroundColor "red" ]`


## Anti-features

- Mation is currently in development and the API is not solidified

- Currently Mation is not the best choice for performance-sensitive applications


## Docs

You can serve the current docs with

```bash
nix run github:quelklef/mation/main#docs
```

or just build them with

```bash
nix build github:quelklef/mation/main#docs
```
