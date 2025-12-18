# Test Coverage Report

## Test Modules

### Integration Tests (`dia_tests.erl`)
- **Compilation tests**: Full compilation and selective file compilation
- **Comparison tests**: Generated output validation against expected files
- **Cross-OTP compatibility**: Tests across different Erlang/OTP versions
- **Dependency resolution**: Tests for @inherits directive handling
- **Git-based plugin loading**: Real-world usage pattern simulation

### Unit Tests (`unit_tests.erl`)
- **Provider initialization**: Tests for both compile and clean providers
- **Configuration parsing**: dia_opts, dia_first_files, dia_only_files
- **Path handling**: Filename operations and edge cases
- **Dependency graphs**: Simple and complex dependency resolution
- **File pattern matching**: Regex for .dia file discovery
- **Error formatting**: Proper error message generation
- **Rebar3 state operations**: State manipulation and configuration

### Error Tests (`error_tests.erl`)
- **Malformed files**: Empty files, comments-only, syntax errors
- **Missing dependencies**: Non-existent inherited dictionaries
- **File system errors**: Non-existent files, permission issues
- **Circular dependencies**: Detection of circular inheritance
- **Name conflicts**: Handling @name vs filename mismatches
- **Encoding**: UTF-8 content handling
- **Edge cases**: Long lines, no trailing newlines, Windows line endings

### Property-Based Tests (`property_tests.erl`)
- **Graph properties**: Vertex count preservation, acyclic properties
- **Idempotent operations**: Filename operations
- **List operations**: Deduplication element preservation
- **Fuzz testing**: Random input handling
- **Invariants**: Dependency ordering, reachable nodes
- **Stress testing**: Large graphs (1000+ nodes), many file operations

## Test Files

### Diameter Dictionary Files
- `foo.dia` - Simple dictionary
- `bar.dia` - Another simple dictionary
- `baz.dia` - Test dictionary
- `qux.dia` - **NEW** Dictionary with single inheritance (from foo)
- `complex.dia` - **NEW** Dictionary with multiple inheritance (from foo and bar)
- `diameter_3gpp_base.dia` - Real-world 3GPP base dictionary

### Expected Output Files
- `test/expected/src/diameter_3gpp_base.erl-expected` - Expected .erl output
- `test/expected/include/diameter_3gpp_base.hrl-expected` - Expected .hrl output

## Running Tests

### All Tests
```bash
rebar3 eunit
```

### Specific Module
```bash
rebar3 eunit --module=unit_tests
rebar3 eunit --module=error_tests
rebar3 eunit --module=property_tests
```

### With Coverage
```bash
rebar3 cover -v
```

### Integration Tests Only
```bash
rebar3 eunit --module=dia_tests
```

## Coverage Goals

- **Line Coverage**: >80%
- **Branch Coverage**: >70%
- **Function Coverage**: >90%

## Test Categories

### ✅ Currently Covered
- Basic compilation and cleaning
- Dependency resolution
- File generation and comparison
- Provider initialization
- Configuration options
- Simple error cases

### ✅ NEW Coverage Added
- Malformed .dia file handling
- Circular dependency detection
- Missing dependency handling
- File system errors
- UTF-8 and encoding issues
- Complex inheritance chains
- Property-based testing
- Large-scale stress testing
- Fuzz testing for robustness

### 🔄 Potential Future Coverage
- Plugin hooks integration
- Multiple app compilation
- Custom output directories
- Incremental compilation
- Watch mode (if implemented)
- Performance benchmarks

## CI/CD Integration

Tests run automatically on:
- Pull requests
- Main branch pushes
- Multiple OTP versions (see .github/workflows/erlang.yml)

## Notes

- Property tests require PropEr library (optional)
- Integration tests create temporary rebar3 projects
- Tests are compatible with OTP 23+
- Some tests may take longer due to compilation overhead
