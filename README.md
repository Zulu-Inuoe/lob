# lob

A command-line build tool for Common Lisp

[![Actions Status](https://github.com/Zulu-Inuoe/lob/workflows/ci/badge.svg)](https://github.com/Zulu-Inuoe/lob/actions)

## Why

I've found myself writing similar tools in various projects and I think it's time I dedicate more time and make it a bit more robust.

## Status

- [x] Build SBCL executables specifying systems/files & entry point
- [x] Specify ASDF 'include' path (-I)
- [x] `:lob` in `*features*` when building specified files
- [x] ASDF package-inferred-systems support [1]
- [ ] REPL/Development mode [2]
- [ ] [SLIME][SLIME]/[Sly][Sly] integration [3]
- [x] Inferred entry point [4]
- [ ] Versioning support [5]
- [?] Project generation
- [ ] Build without ASDF [6]
- [ ] Slynk/Swank support [7]
- [ ] Shrinkwrapping & statically linking libraries to SBCL
- [ ] Utilities for dealing with native libraries (building/packaging?)

[1] I'd like to ~~hijack~~use ASDF package-inferred-systems in order to automatically load dependent systems implied by a lisp file specifically given to `lob`
This would allow a single file `pong.lisp` and have it automatically load dependencies without an explicit defsystem

[2] Spin up a repl with the given ASDF configuration, loaded systems, and initial package(?)

[3] Slin up [SLIME][SLIME]/[Sly][Sly] with the given development environment in a 'simple' way (a meta file ala qlot?)

[4] It might make sense to be smarter about inferring an entry point. Currently, `cl-user::main` is the default, with the assumption that a `main.lisp` file guarded behind a `:if-feature :lob` defines `cl-user::main`
Instead, we could use the given loaded files to figure out a reasonable package name.
eg. Given the command

``` shell
lob pong.lisp
```
Assume `pong::main` as the entry point.

For

``` shell
lob config.lisp pong.asd
```
It would still assume `pong::main`

This has the benefit that if the user prefers to define `main` in a separate package/file, they could:
``` shell
lob pong.asd pong-main.lisp
```
.. rather than including the executable entry point in their 'real' system/package.

[5] It's possible to manually specify individual directories to look for ASDF systems, either through [ASDF itself](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html) or via the command-line via `-I`. But this implies knowledge over the build environment, leading to more specific environment configuration, or more complicated build scripts to pull dependencies.
This is fine for 'local' dependencies bundled in a project, but not for third-party dependencies pulled from [Quicklisp][Quicklisp] or [Ultralisp][Ultralisp].

[Qlot][Qlot] is an attempt at solving this problem. Qlot takes an [npm](https://www.npmjs.com/)-like approach of:
* Specify project via a manifest file `qlfile`
* On first 'pull', fully resolve dependency URLs/versions into `qlfile.lock`
* pull dependencies in a local 'fake' [Quicklisp][Quicklisp] directory ala `node_modules/`

A full project-local approach has some advantages in isolation, but some disadvantages in duplicated dependency sources for each `qlfile` on the machine.
Second, the `qlot` environment is 'married' to Quicklisp in this scheme

It's worth investigating to see if a user-wide, version-aware system registry is viable, perhaps running on top of qlot (as the `.qlot/` directory is not a fundamental design feature of [Qlot][Qlot] itself.

[6] It should be possible to make ASDF-less builds, even when the things to load are systems.
This can be accomplished by using ASDF to generate a load plan or bundle, and ask the new image to load that.

[7] It should be possible to have lob 'hoist' the resulting executable with [swank][SLIME]/[slynk][Sly] support.
This should be possible not just for debugging and interactive development (the REPL feature is better in this case), but for further development once an application is deployed.

Of course users can manually configure such a server in their application logic, but this is boilerplate that can be automated at the build level instead.

# Things to consider

A list of other things I want to look at:

- [defmain](https://40ants.com/lisp-project-of-the-day/2020/06/0090-defmain.html)
- [lime](https://40ants.com/lisp-project-of-the-day/2020/06/0092-lime.html)

# Other Projects

You're likely better off using one of these instead, which do similar (and far more) of the same:

* [BuildApp](https://github.com/xach/buildapp)
* [Roswell](https://github.com/roswell/roswell)
* [QuickProject](https://github.com/xach/quickproject)
* [Qlot][Qlot]
* [cl-project](https://github.com/fukamachi/cl-project)

# License
See [LICENSE](LICENSE.txt)

[SLIME]: [https://common-lisp.net/project/slime/]
[Sly]: [https://github.com/joaotavora/sly]
[Quicklisp]: https://www.quicklisp.org/
[Ultralisp]: https://ultralisp.org/
[Qlot]: https://github.com/fukamachi/qlot
