# WhileLangParser
OCaml parser of While-Language

## Example Run
```
make exampleRun
```

## Prerequisites
* OCaml
* Menhir

## Usage
```
# Install Menhir using opam
make install-deps

# Example run
make exapmleRun

# Run for the file "file.txt"
make
./main.native file.txt
```

## Notice
* As you can see in `exapmle1.txt`, every program text input should be end with the character '#'.
* Some undefined grammar components (such as "cons", "[]", "#", ",") are supported in limited manner.
* If you want to use parser as an OCaml library, consider the function `Parse.parse` in `src/parse.ml` .
