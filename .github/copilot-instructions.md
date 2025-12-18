# Copilot Instructions for rebar3_diameter_compiler

## Project Overview

This is a **rebar3 plugin** that compiles Diameter protocol dictionary files (`.dia`) into Erlang source code. It integrates with rebar3's build system as a provider plugin with compile and clean tasks.

### Architecture

- **Entry point**: `src/rebar3_diameter_compiler.erl` - Minimal wrapper that registers two providers
- **Compile provider**: `src/compile_diameter.erl` - Core compilation logic with dependency resolution
- **Clean provider**: `src/clean_diameter.erl` - Removes generated `.erl` and `.hrl` files
- **Key mechanism**: Uses digraph for topological sorting of `@inherits` dependencies in `.dia` files

### Critical Data Flow

1. Discover `.dia` files in `dia/` directory using regex pattern `^(?!\\._).*\\.dia$`
2. Parse each file to extract `@inherits` directives (binary pattern matching on `<<"@inherits">>`)
3. Build dependency graph using `digraph` module
4. Topologically sort files so inherited dictionaries compile first
5. Generate `.erl` files (to `src/`) and `.hrl` files (to `include/`) via `diameter_codegen`
6. Compile generated `.erl` to `.beam` and load into code path

## Developer Workflows

### Testing (59 tests across 4 modules)

```bash
rebar3 eunit                         # Run all tests
rebar3 eunit --module=unit_tests     # Unit tests only
rebar3 eunit --module=error_tests    # Error handling tests
rebar3 eunit --module=property_tests # Fuzz/stress tests
rebar3 eunit --module=dia_tests      # Integration tests
GOLDEN_RUN=1 rebar3 eunit           # Regenerate expected test outputs
```

**Test structure**: Integration tests (`dia_tests.erl`) compile real rebar3 projects in `_build/test/lib/`. They use git-based plugin loading with `file://` URLs pointing to repo root.

**CI Testing**: Tests detect CI environments via `GITHUB_HEAD_REF` and `GITHUB_REF_NAME` environment variables for proper git branch handling. Integration tests capture full command output with exit codes for debugging failures.

### Code Quality

```bash
rebar3 lint           # edoc + format + xref
rebar3 lint_all       # + dialyzer + dialyzer_html
rebar3 format         # Format with erlfmt (100 char width)
```

### Publishing

```bash
rebar3 pub            # eunit + edoc + format + hex cut
rebar3 publish_hex    # eunit + edoc + hex publish
```

### Nix Development

```bash
nix develop          # Enter dev shell with Erlang + rebar3
nix build            # Build package
nix run .#test       # Run tests via Nix
```

## Project-Specific Patterns

### Provider Registration Pattern

All providers follow this structure:

1. Export `init/1`, `do/1`, `format_error/1`
2. Use `providers:create/1` with namespace `diameter` and module `?MODULE`
3. Register via `rebar_state:add_provider/2`
4. Dependency: `[{default, app_discovery}]` to ensure apps are discovered first

### Dependency Resolution Algorithm

**Key insight**: The `compile_order/3` function in `compile_diameter.erl` uses a sophisticated graph approach:

- Parses `.dia` files as binaries split by newlines to extract `@inherits` directives
- Builds digraph where edges point FROM dependent TO dependency
- Uses `digraph_utils:reachable/2` + `digraph_utils:subgraph/2` + `digraph_utils:topsort/1`
- Filters by `dia_only_files` config option to compile subset of files
- Returns `{ok, Order}` or `{error, Reason}` (circular deps return `false` from topsort)

### Configuration Options

Three rebar.config options (see `compile_diameter.erl` lines 147-152):

- `dia_opts` - Passed to `diameter_dict_util:parse/2` and `diameter_codegen:from_dict/4`
- `dia_first_files` - Files to compile before dependency-ordered compilation
- `dia_only_files` - Filter to compile only specific dictionaries (matched by basename)

### Test Patterns

- **Integration tests** (`dia_tests.erl`): Create temp rebar3 projects, setup git branches dynamically for CI
- **Unit tests** (`unit_tests.erl`): Test provider init, graph algorithms, path handling
- **Error tests** (`error_tests.erl`): Malformed files, encoding, circular deps
- **Property tests** (`property_tests.erl`): Fuzz testing, stress tests (1000+ nodes), PropEr integration (optional)

**Testing anti-pattern**: Don't call providers directly. Integration tests use actual `rebar3 diameter compile` commands via `os:cmd/1`.

### EDoc Documentation

Heavy use of `@doc`, `@private`, `@param`, `@returns` tags. The `edoc_opts` in rebar.config generates docs to `doc/` with custom overview from `doc/overview.edoc`.

### Cross-OTP Compatibility

Tests filter OTP version differences like `-moduledoc(false).` (OTP 27+). See `filter_otp_differences/1` in `dia_tests.erl`.

## Key Files to Reference

- `src/compile_diameter.erl` lines 301-393: Complete dependency resolution algorithm
- `test/dia_tests.erl` lines 88-222: CI-aware test setup with git branch detection
- `rebar.config` lines 11-16: Custom command aliases pattern
- `test/TEST_COVERAGE.md`: Comprehensive test documentation

## External Dependencies

- **diameter** (OTP standard library): Uses `diameter_dict_util` for parsing, `diameter_codegen` for code generation
- **rebar3**: Provider API, state management, base_compiler
- **digraph**: Dependency graph construction (standard library)
- No external deps in `rebar.config` - pure OTP plugin

## Common Pitfalls

1. **Graph direction matters**: Edges go FROM dependent TO dependency, not vice versa
2. **Binary parsing**: `.dia` files split by `<<"\n">>` and `<<"\r">>`, not strings
3. **Test isolation**: Integration tests must use unique temp dirs with `erlang:unique_integer([positive])`
4. **Provider hooks**: Use `{pre, [...]}` not `{post, [...]}` for compile/clean hooks
5. **Circular deps**: `digraph_utils:topsort/1` returns `false` (not error tuple) for cycles
