# cmus-web

Incomplete web interface for cmus library and playback. 

Proof of concept frontend exists that toggles pause/play. Expect more when I have time.

Requires `stack` and `spago` to be installed for Haskell and Purescript building respectively.

```
make
# may have to be root for the following
make install
make uninstall
```

- Run `cmus` as normal. Use `set softvol=true` to control volume.
- Run `cmus-web-server` with an optional port number of choice (default 1917). Navigating to `localhost:1917` downloads the frontend.
- Directly controls cmus through shell commands. You should see changes reflected in your local cmus session.
