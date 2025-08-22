# Hex.pm and GitHub Pages Setup Guide

This guide explains how to use the configured hex.pm integration and GitHub Pages documentation publishing.

## Prerequisites

1. **Hex.pm Account**: Create an account at [hex.pm](https://hex.pm)
2. **Hex API Key**: Generate an API key from your [hex.pm account](https://hex.pm/dashboard/keys)
3. **GitHub Repository Settings**: Enable GitHub Pages

## Setting up Hex.pm Publishing

### 1. Add Hex API Key to GitHub Secrets

1. Go to your GitHub repository
2. Navigate to Settings → Secrets and variables → Actions
3. Click "New repository secret"
4. Name: `HEX_API_KEY`
5. Value: Your hex.pm API key

### 2. Enable GitHub Pages

1. Go to Settings → Pages
2. Source: "GitHub Actions"
3. Save

## Publishing Workflow

### Automatic Publishing (Recommended)

The repository is configured for automatic publishing:

**When you push a tag:**

```bash
git tag v0.8.1
git push origin v0.8.1
```

This triggers:

1. Running all tests
2. Building the hex package
3. Generating documentation
4. Publishing to hex.pm

**When you push to master:**

1. Tests run on multiple OTP versions
2. Documentation is generated and published to GitHub Pages

### Manual Publishing

You can also publish manually:

```bash
# Test everything first
rebar3 eunit

# Build and publish to hex.pm
rebar3 publish_hex

# Or step by step:
rebar3 hex build    # Creates the package tarball
rebar3 hex publish  # Publishes to hex.pm
```

## Documentation

### Automatic Documentation Publishing

Documentation is automatically:

1. **Generated**: Using EDoc from source code comments
2. **Published to GitHub Pages**: Available at `https://carlosedp.github.io/rebar3_diameter_compiler/`
3. **Included in Hex package**: Shows on the hex.pm package page

### Manual Documentation Generation

```bash
# Generate local documentation
rebar3 edoc

# Documentation will be in ./doc/ directory
open doc/index.html
```

## Package Information

The package includes comprehensive metadata:

- **Description**: Extracted from app.src
- **Dependencies**: Automatically detected
- **Documentation**: EDoc generated
- **Links**: GitHub repository and documentation
- **License**: MIT

## Troubleshooting

### Common Issues

**Authentication Error:**

- Verify `HEX_API_KEY` secret is set correctly
- Check API key hasn't expired

**Documentation Build Fails:**

- Ensure all EDoc comments are properly formatted
- Check for syntax errors in overview.edoc

**GitHub Pages Not Updating:**

- Check Actions tab for build errors
- Verify Pages settings are correct

### Checking Package Status

```bash
# View package information
rebar3 hex info rebar3_diameter_compiler

# View local package details
rebar3 hex build
```

## Version Management

When releasing a new version:

1. Update version in `src/rebar3_diameter_compiler.app.src`
2. Update `CHANGELOG.md` (if you have one)
3. Commit changes
4. Create and push tag:

   ```bash
   git tag v0.8.1
   git push origin v0.8.1
   ```

The GitHub Action will handle the rest!
