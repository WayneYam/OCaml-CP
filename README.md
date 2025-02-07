# OCaml-CP

Ocaml in competitive programming. Requires online judges with support for some common packages like `Base` or `Core`. Known judges that work are DMOJ and AtCoder.

# Directory Structure

## Templates

`bin/dune` and `bin/template.ml` are the default build configuration and the default code

## Problem Library

Other files in `bin` are the solutions to some problems, check the relavant commits for detail.

## Algorithm Library

`lib` contains some commonly used code

# Workflow

The main problem is online judges only support a single file upload, and they do not support ppx, so I have to do that before submitting

1. Use `oinit.fish` to generate the files for `<problem>`
2. Write code
3. Use `dune build <problem>.exe` and `dune exec <problem>.exe` to build and run the code
4. Use `dune build <problem>_include.exe` to generate the code that can be submitted directly, which I pipe to `wl-copy` in order to submit
