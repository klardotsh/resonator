## Linux (Bare Metal)

> These instructions are written on Void Linux, however should be roughly
> portable across distributions (with package names changed as appropriate - for
> example, Arch and Gentoo don't have `-dev/-devel` packages, but bundle headers
> into the main packages)

### You'll need the following dependencies to *build* resonator:

- stack
- ghc
- file-devel (on most other distributions this is libmagic-devel)

You can then build resonator with `stack build`

### You'll need the following dependencies to *run* resonator:

- ffmpeg

## Notes to packagers

- `config.example.toml` should ideally be installed to
  `/etc/skel/resonator/config.toml` or
  `/usr/share/doc/resonator/config.example.toml`, or some roughly equivalent
  path on your distribution. Please make it available to your end users; even if
  they *can* go scouring through the internet to find out what
  `clients.max-bitrate-exceeded-request-behavior` is supposed to mean, ideally,
  they shouldn't have to
