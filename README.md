rebar3_diameter_compiler
=====

Compile [diameter](http://erlang.org/doc/man/diameter.html) .dia files in rebar3 projects

[![Erlang CI](https://github.com/carlosedp/rebar3_diameter_compiler/actions/workflows/erlang.yml/badge.svg)](https://github.com/carlosedp/rebar3_diameter_compiler/actions/workflows/erlang.yml)
 [![Hex pm](http://img.shields.io/hexpm/v/rebar3_diameter_compiler.svg?style=flat)](https://hex.pm/packages/rebar3_diameter_compiler)

Build
-----

    ./rebar3 compile

Use
---

Add the plugin to your rebar config from Github:

```erlang
{plugins, [
    { rebar3_diameter_compiler, {git, "https://github.com/carlosedp/rebar3_diameter_compiler.git", {branch, "master"}}}
]}.

{provider_hooks, [
    {pre, [
        {compile, {diameter, compile}},
        {clean, {diameter, clean}}
    ]}
]}.
```

Or fetch the plugin using Hex.pm:

```erlang
{plugins, [
    rebar3_diameter_compiler
]}.

{provider_hooks, [
    {pre, [
        {compile, {diameter, compile}},
        {clean, {diameter, clean}}
    ]}
]}.
```


The plugin will be ran on compile and clean commands or call your plugin directly in an existing application:

    $ rebar3 diameter compile
    ===> Fetching rebar3_diameter_compiler
    ===> Compiling rebar3_diameter_compiler
    ===> Compiling diameter...

    $ rebar3 diameter clean
    ===> Cleaning diameter compiled files...

Test
-----

This test compiles a `.dia` file for validation

    rebar3 eunit

Publishing new versions
-----

This is mostly a reminder on how to bump and publish a new version.

    # Login to Hex
    rebar3 hex user auth
    # Cut new version with hex
    rebar3 hex cut
