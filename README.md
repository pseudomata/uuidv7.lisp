# `uuidv7.lisp`

This repository contains a **Common Lisp** implementation for **`UUIDv7`**. See the
[RFC4122 Version 14 Draft](https://datatracker.ietf.org/doc/draft-ietf-uuidrev-rfc4122bis/) for the
specification or this [IETF document](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7). I'd also encourage reading [Buildkite](https://buildkite.com/)'s blog post ["Goodbye integers. Hello UUIDv7!"](https://buildkite.com/blog/goodbye-integers-hello-uuids).

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

- `sbcl`
- `quicklisp` (https://www.quicklisp.org/beta/)

Getting started:
```
CL-USER> (require :asdf)
("uiop" "UIOP" "asdf" "ASDF")
CL-USER> (load "~/quicklisp.lisp")

  ==== quicklisp quickstart 2015-01-28 loaded ====

    To continue with installation, evaluate: (quicklisp-quickstart:install)

    For installation options, evaluate: (quicklisp-quickstart:help)

T
CL-USER> (quicklisp-quickstart:install)
CL-USER> (asdf:load-system :uuidv7)
```

You can run tests by loading `:uuidv7-test`:
```
CL-USER> (asdf:load-system :uuidv7-test)
; compilation finished in 0:00:00.023

Running test suite TEST-UUIDV7
 Running test GENERATE-IDS ..................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
 Running test STRING-TO-BYTES-CONVERSION .
 Did 2003 checks.
    Pass: 2003 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
```

## Usage

```lisp
CL-USER> (uuidv7:generate)
#(1 140 100 145 3 33 123 133 190 24 102 255 181 138 226 217)
CL-USER> (uuidv7:bytes->string #(1 140 100 145 3 33 123 133 190 24 102 255 181 138 226 217))
"018C6491-0321-7B85-BE18-66FFB58AE2D9"
CL-USER> (uuidv7:string->bytes "018C6491-0321-7B85-BE18-66FFB58AE2D9")
#(1 140 100 145 3 33 123 133 190 24 102 255 181 138 226 217)
```

## License

Mozilla Public License, v. 2.0. See [LICENSE](./LICENSE).

```
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
```
