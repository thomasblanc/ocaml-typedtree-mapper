ocaml-typedtree-mapper
======================

These files provide classes that can go through a typedtree.

Pretty simply, Tt_mapper provides a class "mapper" that does a map and Tt_iter a class "iterator" that iterates along the typedtree. The two files can be compiled and used independantly.

The branches of the tree are visited in a simple order: the one provided syntactically in the definition of the typedtree (see the source of the OCaml compiler for details) with the exception of sum types in the mapper.

You can have a simpler mapping or iterator in the official compiler libs.

Note that there is neither a META or a building/installing file.
It's because I don't see that (tiny) work as a real library.
You can simply copy and compile the files inside your projects.

Compilation needs -I /ocamlsources/typing/ -I /ocamlsources/parsing/ and linking needs ocamlcommon.cma.

It is advised to have a good level in OCaml and compiler internals before using this tool.
Have fun !

Thomas Blanc
