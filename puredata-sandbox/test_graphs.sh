OCAMLRUNPARAM=b find . -name "*.pd" -print -exec ../main.native -de  --connect-subpatches  {} \;
