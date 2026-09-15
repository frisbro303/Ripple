# Ripple

[![CI](https://github.com/frisbro303/ripple/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/frisbro303/ripple/actions/workflows/ci.yml)
[![Latest release](https://img.shields.io/github/v/release/frisbro303/ripple?color=blue)](https://github.com/frisbro303/ripple/releases/latest)
[![License: GPL v3](https://img.shields.io/github/license/frisbro303/ripple?color=blue)](LICENSE)

An opinionated spaced-repetition flashcard tool. 
No deck-management procrastination - edit cards as you review, 
so your time stays focused on what matters: learning.

## Installation

Download the latest macOS build from the [releases page](https://github.com/frisbro303/ripple/releases/latest).

### Building from source

Requires [Node.js](https://nodejs.org/) and a [Rust toolchain](https://www.rust-lang.org/tools/install). See the [Tauri prerequisites guide](https://v2.tauri.app/start/prerequisites/) for platform-specific system dependencies.

```bash
npm install
npx tauri dev    # run in development mode
npx tauri build  # produce a release bundle in src-tauri/target/release/bundle
```

Run the tests and linter with:

```bash
npm test
npm run review
```

## License
Ripple is licensed under the GPL-3.0 license. See [LICENSE](LICENSE) for details.
