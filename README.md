# `uuidv7.lisp`

This repository contains a **Common Lisp** implementation for **`UUIDv7`**. See the [IETF document](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7) for the specification. I'd also encourage reading [Buildkite](https://buildkite.com/)'s blog post ["Goodbye integers. Hello UUIDv7!"](https://buildkite.com/blog/goodbye-integers-hello-uuids).

⬇️ _(Relevant overview reproduced from the IETF document below)_

UUID version 7 features a time-ordered value field derived from the widely implemented and well known Unix Epoch timestamp source, the number of milliseconds seconds since midnight 1 Jan 1970 UTC, leap seconds excluded.

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                           unix_ts_ms                          |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|          unix_ts_ms           |  ver  |       rand_a          |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|var|                        rand_b                             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                            rand_b                             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```
_Figure 3: UUIDv7 Field and Bit Layout_

| Field      | Description |
|------------|-------------|
| unix_ts_ms | 48 bit big-endian unsigned number of Unix epoch timestamp |
| ver        | 4 bit UUIDv7 version |
| rand_a     | 12 bits pseudo-random data to provide uniqueness |
| var        | The 2 bit variant |
| rand_b     | The final 62 bits of pseudo-random data to provide uniqueness |

## Development

...

## Usage

...

## License

Mozilla Public License, v. 2.0. See [LICENSE](./LICENSE).

```
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
```
