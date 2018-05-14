test:
	ocamlbuild -use-ocamlfind -I src test_file.native && ./test_file.native

repl:
	ocamlbuild -use-ocamlfind -I src main.native && ./main.native

compile:
	ocamlbuild -use-ocamlfind user.cmo crypto.cmo bs.cmo main.cmo

zip:
	zip CS3110-BlockChain.zip *.ml*

clean:
	ocamlbuild -clean
	rm -f CS3110-BlockChain.zip
