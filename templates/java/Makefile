main: source clean
	javac ./Main.java
clean:
	rm -f ./*.class
source:
	cp ./Main.java /tmp/source
check-syntax:
	javac -Xlint $(CHK_SOURCES)
run:
	cat ./inp | java Main > ./outp
edit:
	vim ./Main.java
