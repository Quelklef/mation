# Installing Mation

## With spago

Frankly I don't know because I don't use spago. But I'll find out for all you lovely readers


## With [`purs-nix`](https://github.com/purs-nix/purs-nix)

First install `purs-nix` however you like. Then use `purs-nix.build` to create a `mation` package, as in the nix expression below. The result can be used as any other `purs-nix` package; for instance, you can place it in a `dependencies` array.

```nix
{ purs-nix }: let

mation =
  purs-nix.build {
    name = "mation";
    src.path =
      pkgs.fetchFromGitHub
        { owner  = "quelklef";
          repo   = "mation";
          rev    = REPLACE_ME;
          sha256 = REPLACE_ME;
        };
    info = /package.nix;
  }

my-package = purs-nix.purs
  { /* ... */
    dependencies = [ /* ... */ mation ]
  };

in /* ... */
```

