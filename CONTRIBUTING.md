# Contributing to FastPlaz

Thank you for your interest in FastPlaz! Bug reports, code, tests, documentation and examples are all welcome.
The people behind the project are listed in [CONTRIBUTOR.md](CONTRIBUTOR.md).

## Table of contents

- [Reporting bugs and requesting features](#reporting-bugs-and-requesting-features)
- [Development setup](#development-setup)
- [Workflow](#workflow)
- [Coding guidelines](#coding-guidelines)
- [Tests](#tests)
- [Commit messages](#commit-messages)
- [Pull requests](#pull-requests)

## Reporting bugs and requesting features

Before opening an issue, search the [existing issues](https://github.com/fastplaz/fastplaz/issues) to avoid duplicates.

A good bug report includes:

- FPC and Lazarus versions (for example FPC 3.2.2, Lazarus 3.x)
- Operating system and architecture
- How the application runs: CGI (with the web server used) or standalone HTTP
- The smallest code or steps that reproduce the problem
- What you expected and what happened instead (error messages, logs)

For feature requests, describe the problem you want to solve, not only the solution.

> **Security issues:** please do not open a public issue. Contact the maintainers privately first.

## Development setup

Requirements:

- **Free Pascal 3.2.2**
- **Lazarus** (for `lazbuild` and the IDE packages)
- `fcl-web` (part of FPC)
- `jcfbase` and `jcfidelazarus` (Jedi Code Format), installed through the Lazarus Online Package Manager; needed to compile `fastplaz_tools.lpk`

Install the packages:

1. Open `tools/fastplaz_runtime.lpk` and compile it.
2. Open `tools/fastplaz_tools.lpk`, compile it and install it into the IDE.

See [README-package.md](README-package.md) for the optional packages.

## Workflow

1. **Fork** [fastplaz/fastplaz](https://github.com/fastplaz/fastplaz) and clone your fork.
2. Create a branch from **`development`**:
   ```bash
   git checkout development
   git pull
   git checkout -b feature/my-feature     # or fix/short-description
   ```
3. Make your changes, with tests where possible.
4. Check that everything compiles and the tests pass (see [Tests](#tests)).
5. Push the branch to your fork.
6. Open a **Pull Request against `development`**. Do not target `master`; it only receives tested releases.

## Coding guidelines

- **Cross-platform.** FastPlaz runs on Windows, Linux and macOS. Avoid OS-specific code, or wrap it in the proper `{$IFDEF}` blocks.
- **Backward compatible.** Many production applications use FastPlaz. Do not change a public API without a strong reason and a migration note.
- **Both run modes.** Code in the request path must work in CGI and in standalone HTTP mode. A standalone process serves many requests, so do not leak memory or keep request state in globals.
- **Follow the existing style.** Match the formatting and naming of the surrounding unit (`T` prefix for types, `F` for private fields).
- **Keep it focused.** One pull request should address one topic. Avoid unrelated reformatting.
- **Security.** Escape output (HTML, SQL, headers), validate input, and do not trust client-supplied headers by default.
- **Document it.** New features should come with documentation or an example in [`examples/`](examples).

## Tests

Unit tests use **FPCUnit** and run as plain console programs, without installing anything into the IDE.
The quickest way is `instantfpc`, which ships with FPC:

```bash
# Linux / macOS
sh FastPlaz/test/run_tests.sh
sh tools/src/test/run_tests.sh
```

```bat
:: Windows
FastPlaz\test\run_tests.bat
tools\src\test\run_tests.bat
```

- A bug fix should come with a test that fails before the fix.
- A new unit or feature should come with tests for its main behaviour.
- Some suites need optional services (Redis, the SQLite library) and are skipped when they are not available.

All test projects, options and details are described in [UNIT_TEST.md](UNIT_TEST.md).

## Commit messages

Write commit messages in **English**. Start with the affected unit or area, followed by a short summary in the imperative mood:

```text
routes: anchor the built-in route patterns
session_controller: regenerate the id after login
docs: describe the standalone deployment
```

Add a body when the reason for the change is not obvious from the summary.

## Pull requests

Before opening a pull request, check that:

- [ ] the branch is based on `development` and the PR targets `development`
- [ ] the runtime package (`tools/fastplaz_runtime.lpk`) compiles
- [ ] the tests pass, and new tests cover the change
- [ ] documentation or examples are updated when behaviour changes
- [ ] the description explains **what** changed and **why**, and links related issues

Maintainers may ask for changes during review. Small, focused pull requests are reviewed faster.

---

Thank you for helping make FastPlaz better! ❤️
