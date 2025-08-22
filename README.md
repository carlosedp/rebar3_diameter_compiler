# rebar3_diameter_compiler

Compile [diameter](http://erlang.org/doc/man/diameter.html) `.dia` files in rebar3 projects.

Browse the [plugin documentation](https://carlosedp.github.io/rebar3_diameter_compiler/).

[![Erlang CI](https://github.com/carlosedp/rebar3_diameter_compiler/actions/workflows/erlang.yml/badge.svg)](https://github.com/carlosedp/rebar3_diameter_compiler/actions/workflows/erlang.yml)
 [![Hex pm](http://img.shields.io/hexpm/v/rebar3_diameter_compiler.svg?style=flat)](https://hex.pm/packages/rebar3_diameter_compiler)

## Build

    ./rebar3 compile

## Use

Add the plugin to your rebar config from Hex.pm (published version):

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

Or fetch the plugin using Github (to use the latest pushes):

```erlang
{plugins, [
    { rebar3_diameter_compiler, {git, "https://github.com/carlosedp/rebar3_diameter_compiler.git", {branch, "main"}}}
]}.

{provider_hooks, [
    {pre, [
        {compile, {diameter, compile}},
        {clean, {diameter, clean}}
    ]}
]}.
```

The plugin will run on compile and clean commands (like `rebar3 compile` and `rebar3 clean`). You can also call your plugin directly in an existing application:

    $ rebar3 diameter compile
    ===> Fetching rebar3_diameter_compiler
    ===> Compiling rebar3_diameter_compiler
    ===> Compiling diameter...

    $ rebar3 diameter clean
    ===> Cleaning diameter compiled files...


The plugin will compile all `.dia` files by default. You can use the option below to compile only specific diameter dictionaries

```erlang
{dia_only_files, [
    your_dictionary_name
    ]}.
```

## Testing

One test compiles a `.dia` file for validation, another compares the generated erl/hrl files with the expected files in `test/expected` directory.

    rebar3 eunit

If the format changes in the future(for example due a compiler change), the expected files can be re-generated with `GOLDEN_RUN=1 rebar3 eunit`. This will overwrite the existing expected files.

The test filters out simple differences between OTP versions like blank lines and `-moduledoc(false).` in more recent versions.

## Publishing new versions to hex.pm

This is mostly a reminder on how to bump and publish a new version.

    # Reformat code and generate docs
    rebar3 do edoc, format
    # Run tests
    rebar3 eunit
    # Login to Hex
    rebar3 hex user auth
    # Cut new version with hex
    rebar3 hex cut
