ocaml-typedtree-mapper
======================

An object doing exhaustive mapping of typedtrees.

To use this object, simply create a new class inheriting Tt_mapper.mapper and call one of the entry functions.
If you want your class to effectively do something on the tree, you'll have to overide the corresponding method.
Make sure you still do the mapping on the node's sons.

As of right now there is no warranty on the order in wich ther branch will be evaluated.
You can have a pretty simpler mapping (and also an iterator) in the official compiler libs.

It is advised to have a good level in OCaml and compiler internals before using this tool.
Have fun !

Thomas Blanc
