# bin-annot

Like [ocaml-annot](https://github.com/avsm/ocaml-annot), but for binary annotation files (cmt).

If you compile your OCaml code with the `-bin-annot` option, `bin-annot -type l c file.cmt`
can tell what's the type of the thing at line `l`, column `c` of `file.ml`.

## Editor integration

It is the same command line than `ocaml-annot` so that [editor configuration](https://github.com/avsm/ocaml-annot/blob/master/README) should stay the same.

## Build

You need `findlib` aka `ocamlfind`, and `ocaml-compiler-libs` (both available on opam).
Then:

`make && make install`
