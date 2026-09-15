# Ripple

[![CI](https://github.com/frisbro303/ripple/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/frisbro303/ripple/actions/workflows/ci.yml)
[![Latest release](https://img.shields.io/github/v/release/frisbro303/ripple?color=blue)](https://github.com/frisbro303/ripple/releases/latest)
[![License: GPL v3](https://img.shields.io/github/license/frisbro303/ripple?color=blue)](LICENSE)
[![Buy Me a Coffee](https://img.shields.io/badge/-Buy%20Me%20a%20Coffee-ffdd00?style=flat&logo=buy-me-a-coffee&logoColor=black)](https://www.buymeacoffee.com/frisbro)

An opinionated spaced-repetition flashcard tool. 
No deck-management procrastination - edit cards as you review, 
so your time stays focused on what matters: learning.

## Installation

Download the latest macOS build from the [releases page](https://github.com/frisbro303/ripple/releases/latest).

Linux and Windows builds aren't set up yet, sorry — the app itself is cross-platform (it's Tauri), so building from source below should work fine in the meantime.

If you give Ripple a try, please [open an issue](https://github.com/frisbro303/ripple/issues) for bugs or ideas, and pull requests are very welcome — including for Linux/Windows release builds.

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

## User Guide

Ripple has no deck browser and no separate edit mode — you review and edit in the same place. Cards are written in [Typst](https://typst.app), so you get real typesetting and math instead of plain text.

### Reviewing

- **Space** — reveal the answer, then rate it Good
- **Enter** — reveal the answer (without rating)
- **1 / 2 / 3 / 4** — rate Again / Hard / Good / Easy, once revealed
- **i** / **o** — edit the front / back of the current card (o only once revealed)
- The **⋯** menu on a card lets you push it back to learning, defer it, or delete it

### Adding cards

Press **⌘1** (or click **+**) to add a card. Type Typst source for the front, press **Shift+Enter** to jump to the back, **Shift+Enter** again to submit. Both fields render a live preview as you type.

Paste or drag-and-drop an image into either field, or right-click a field to pick a file — it's embedded with the card.

### Typst basics

Card text compiles to an image on every keystroke. A couple of things worth knowing:
- Brackets, quotes, and `$...$` math delimiters auto-close and auto-skip like a code editor, and **Tab** indents/dedents
- Math: `$x^2 + y^2 = z^2$`
- **Settings → Typst → Preamble** runs before every card, so shared setup (e.g. a `#let` shorthand or custom styling) only needs to be written once instead of repeated per card

### Navigating

| Shortcut | Page |
|---|---|
| ⌘1 | Add |
| ⌘2 | Stats |
| ⌘3 | Account |
| ⌘4 | Settings |
| Escape | back to Review (blurs the field you're editing first, if any) |

### Settings

- **Desired retention** — target recall probability the FSRS scheduler aims for; higher means more frequent reviews
- **New cards per day** — cap on how many new (not-yet-reviewed) cards get introduced daily
- **Defer by** — how many days the card menu's "Defer" action pushes a card out
- **Theme** — System / Light / Dark
- **Preamble** — shared Typst setup applied to every card

### Syncing

Ripple works fully offline with no account. Signing in (Account page) syncs your cards across devices in the background, so you can pick up reviews on another machine.

### Backup

Settings → Data → **Export data** / **Import data** — a full JSON dump of your card history, importable on any device, with or without an account.

## License
Ripple is licensed under the GPL-3.0 license. See [LICENSE](LICENSE) for details.
