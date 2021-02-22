# erlang-h3

![Build status](https://github.com/helium/erlang-h3/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/helium/erlang-h3/branch/master/graph/badge.svg)](https://codecov.io/gh/helium/erlang-h3)
[![Hex.pm](https://img.shields.io/hexpm/v/h3)](https://hex.pm/packages/h3)

The `erlang-h3` library provides an Erlang binding for [Uber's
H3](https://uber.github.io/h3) spatial coordinate library.

Install
------

Add `h3` to your `deps` section in `rebar.config`:

``` shell
{deps, [h3]}.
```

Building
--------

Fork the repo and simply use `make` to build the library. You will
need `cmake` installed to build the required h3 core package.

To run the tests run `make test`.

## Cross compilation

Cross-compilation requires the environment variable `ERTS_INCLUDE_DIR`
defined as the target directory containing `erl_nif.h`,
e.g. `ERTS_INCLUDE_DIR=target/usr/lib/erlang/erts-<VERSION>/include`.

Usage
-----

The `h3` module exports most of the functions from the core H3 library.

Like the JavaScript version the Erlang binding uses degrees instead of
radians when converting to and from geo-coordinates to h3 indexes.

Example using `./rebar3 shell` after cloning the repo:

```shell
$ ./rebar3 shell
...

1> Paris = h3:from_geo({48.8566, 2.3522}, 9).
617550903642685439
2> h3:to_string(Paris).
"891fb466257ffff"
3> h3:to_geo(Paris).
{48.857078058197295,2.3529900909549206}

```
