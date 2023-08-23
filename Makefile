.POSIX:

.SUFFIXES:
all:
	stack build --copy-bins --local-bin-path .
	spago bundle-app
install:
	mkdir -p /etc/cmus-web
	mv ./cmus-web-server /usr/local/bin
	mv ./index.js /etc/cmus-web
	cp ./resources/index.html /etc/cmus-web
uninstall:
	rm /usr/local/bin/cmus-web-server
	rm -rf /etc/cmus-web

