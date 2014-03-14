main: source clean
	ghc ./main.hs
	rm ./main.hi
source:
	cp ./main.hs /tmp/source
clean:
	rm -f ./main
run:
	./main < ./inp > ./outp
edit:
	vim ./main.hs
