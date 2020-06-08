# resonator

resonator is a music streaming service for the modern, multi-device era. It aims
to replicate much of the user experience found in proprietary music
library-on-demand streaming services, but in a self-hosted environment that
respects your privacy. It supports multiple clients each streaming music from a
centralized server in varying encodings, and is suitable for use cases ranging
from streaming an album from a mobile phone (at a bitrate that is appropriate
for cellular data plans) all the way up to a shared jukebox at a party
(potentially in lossless formats, if said jukebox is physically near the
server).

This repository contains the protocol definitions, and the reference server
implementation, implemented in Haskell. In the future, the plan is to also have
a few reference client applications:

- a simple URL generator client suitable for consumption by
  [mpv](https://mpv.io/) and similar
- a TUI application inspired by [cmus](https://cmus.github.io/)
- a [libhandy](https://source.puri.sm/Librem5/libhandy)-based GTK+ client
  suitable for desktop and mobile phone use)

resonator is best suited for those with audio collections encoded
[losslessly](https://en.wikipedia.org/wiki/Lossless_compression), though lossy
collections are still supported. In the latter case, it is important that the
server is configured to do as little transcoding as possible, and that client
applications are configured to be as flexible as possible in their encoding
requests - for details, see `CLIENT_SERVER_PROTOCOL.md`, commentary in
`config.example.toml`, and the documentation of the client applications of your
choosing.

## Installation

If your distribution or OS packages resonator, use that package. The following
distributions are known to package resonator (please add to this list if you
package resonator somewhere):

- placeholder

(placeholder) Otherwise, Docker images are provided as `klardotsh/resonator` -
see `INSTALL_DOCKER.md`, `Dockerfile`, and `docker-compose.example.yml` for
details.

Alternatively, if you're familiar with Haskell tooling, `stack build` (or `stack
install`) should get you a working binary. See `INSTALL.md` for more details.

(placeholder) Failing all of the above, reach out in `#resonator:klar.sh` on
Matrix and someone should be able to help you get going.

## Help and Contributing

- fill
- this
- in

## Legal

This implementation of resonator belongs to the commons:

- All code is released under the [AGPL 3.0 and later
  versions](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0))
- All documentation (including the protocol definition), images, web assets, and
  other media are released under [Creative Commons Attribution-ShareAlike
  4.0](https://creativecommons.org/licenses/by-sa/4.0/)
