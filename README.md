compile_diameter
=====

Compile diameter .dia files

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { compile_diameter, ".*", {git, "git@host:user/compile_diameter.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 compile_diameter
    ===> Fetching compile_diameter
    ===> Compiling compile_diameter
    <Plugin Output>
