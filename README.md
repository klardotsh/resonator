> 15 July 2021: This idea started going somewhere, and then didn't. I'm not sure
> if I'll get back to it, so for now, consider it a throwaway implementation of
> how to parse `ffprobe` output and a TOML config file from Haskell 🤷‍♂️.
> I'm not sure whether this still compiles, I haven't bothered to check
> recently.
>
> This implementation has been relicensed from `AGPL-3.0-or-later` to `CC0-1.0`,
> so go to town.

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

`resonator`'s implementation, specification, documentation, artwork, and other
assets are all [Copyfree](http://copyfree.org/), released under the [Creative
Commons Zero 1.0
dedication](https://creativecommons.org/publicdomain/zero/1.0/). Thus, while
upstream contributions are welcomed and encouraged for the benefit of us all,
you are free to use `resonator` for any purpose and in any context.

Contributions to `resonator`'s first-party repositories must be dedicated under
the same terms. By submitting a contribution to a `resonator` project, you
assert the following (this is the [Unlicense
waiver](https://unlicense.org/WAIVER)):

> I dedicate any and all copyright interest in this software to the
> public domain. I make this dedication for the benefit of the public at
> large and to the detriment of my heirs and successors. I intend this
> dedication to be an overt act of relinquishment in perpetuity of all
> present and future rights to this software under copyright law.
>
> To the best of my knowledge and belief, my contributions are either
> originally authored by me or are derived from prior works which I have
> verified are also in the public domain and are not subject to claims
> of copyright by other parties.
>
> To the best of my knowledge and belief, no individual, business,
> organization, government, or other entity has any copyright interest
> in my contributions, and I affirm that I will not make contributions
> that are otherwise encumbered.
