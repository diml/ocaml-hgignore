ocaml-hgignore
--------------

This is a small tool I wrote once to interpret the .hgignore and
.hg/dirstate. The `plop` program takes a list of files as argument and
return the files that are ignored by hg. In my experiments, it was a
lot faster than hg.
