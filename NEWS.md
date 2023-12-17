# Urgrep News

## v0.3.0 (2023-12-17)

### New features
- Add optional `urgrep-xref` feature, which lets Xref use Urgrep to create the
  command for searching in files/directories

### Bug fixes
- Respect buffer-local values of `urgrep-preferred-tools`

### Breaking changes
- `:files` keyword argument in `urgrep-command` and friends is now
  `:file-wildcard`
- `:directory` keyword argument in `urgrep-commnd` and friends is now `:root`,
  and can accept file and/or directory names

---

## v0.2.0 (2023-08-31)

### New features
- Add support for toggling whether to search in hidden files (`M-s h` in the
  search prompt, or `urgrep-search-hidden-files` globally)
- Add `:directory` key to `urgrep-command`, allowing you to specify zero or more
  directories to search in
- `urgrep` builtin for Eshell now supports specifying search directories as
  arguments
- Allow setting the search tool to use on the fly when reading the query
  (`M-s t` in the search prompt)

### Breaking changes
- `urgrep-run-command` now takes `:tool` as an optional key to match `urgrep`

---

## v0.1.1 (2023-06-07)

### Bug fixes
- Fix Eshell integration

---

## v0.1.0 (2023-05-13)

Initial release
