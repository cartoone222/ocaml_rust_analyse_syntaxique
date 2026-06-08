cd analyser_syntax_ocamel/analyser/
dune clean
echo "42" | dune exec analyser
cd ../../
cargo run