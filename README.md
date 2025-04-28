---
lang: en
title: cursor-contraster README
---

# cursor-contraster

`cursor-contraster` is a tiny Emacs utility that automatically generates
and applies a palette of highly distinguishable cursor colors whenever
you change themes, or on demand. It's perfect for modal setups (Evil,
God-mode, etc.) where you want each cursor state to "pop" against any
background.

------------------------------------------------------------------------

## Table of Contents

1.  [Installation](#installation)
2.  [Quick Start](#quick-start)
3.  [Functions & Commands](#functions--commands)
4.  [Customization & Hooks](#customization--hooks)
5.  [Development](#development)
6.  [License](#license)

## Installation


Apply a static list of cursor specs now and on every theme change:

    (cursor-contraster-setup-with-specs
     '((:var evil-normal-state-cursor  :shape box    :index 0)
       (:var evil-insert-state-cursor  :shape bar    :index 2)
       (:var evil-visual-state-cursor  :shape hollow :index 4)
       (:var evil-replace-state-cursor :shape hbar   :index 6)))

Enable global auto-updates without specifying specs:

    (cursor-contraster-mode 1)

## Functions & Commands {#functions--commands}

### cursor-contraster-generate-palette

    (cursor-contraster-generate-palette &optional count) → list of hex strings

Generate `count` evenly spaced HSL colors (defaults to 16).

### cursor-contraster-apply-cursors

    (cursor-contraster-apply-cursors specs &optional palette)

Apply a list of plist specs (`:var`, `:shape`, `:index`) to set cursor
variables. If `palette` is omitted, a fresh one is generated.

### cursor-contraster-setup-with-specs

    (cursor-contraster-setup-with-specs specs)

1.  Immediately applies `specs`.
2.  Registers them on `cursor-contraster-update-hook` so they rerun
    after each theme load.

### cursor-contraster-mode

    (define-minor-mode cursor-contraster-mode
      "Global mode to auto-update contrasting cursor colors on theme changes.")

Toggles adding/removing the `after-load-theme-hook` that regenerates and
reapplies the palette.

## Customization & Hooks {#customization--hooks}

**Hook:** `cursor-contraster-update-hook`\
Run with a single argument (the new palette). You can add custom
behaviors:

    (add-hook 'cursor-contraster-update-hook
                (lambda (palette)
                  ;; reapply only one cursor, or customize further
                  (cursor-contraster-apply-cursors
                   '((:var evil-normal-state-cursor :shape box :index 0))
                   palette))
                t)

Hook ordering: The setup function uses a *buffer-local* hook so multiple
specs lists can coexist without clobbering one another.

## Development

1.  Fork and clone the repo.
2.  Run `M-x byte-compile-file` on `cursor-contraster.el`.
3.  Submit PRs for new features or bug fixes.

## License

This project is licensed under **GPL-3.0-or-later**. See
[LICENSE](./LICENSE) for details.
