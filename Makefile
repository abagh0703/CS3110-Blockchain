test:
	ocamlbuild -use-ocamlfind -I src main.native && ./main.native

repl:
	ocamlbuild -use-ocamlfind -I src main.native && ./main.native

zip:
	zip CS3110-BlockChain.zip *.ml*

clean:
	ocamlbuild -clean
	rm -f CS3110-BlockChain.zip
