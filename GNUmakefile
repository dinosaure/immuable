vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

immuable.hvt.target: | vendors
	@echo " BUILD unikernel/main.exe"
	@dune build --root . --profile=release ./unikernel/main.exe
	@echo " DESCR unikernel/main.exe"
	@dune describe location \
		--context solo5 --no-print-directory --root . \
		./unikernel/main.exe &> immuable.hvt.target

immuable.exe.target: | vendors
	@echo " BUILD bin/immuable.exe"
	@dune build --root . --profile=release ./bin/immuable.exe
	@echo " DESCR bin/immuable.exe"
	@dune describe location \
		--context default --no-print-directory --root . \
		./bin/immuable.exe &> immuable.exe.target

immuable.hvt: immuable.hvt.target
	@echo " COPY immuable.hvt"
	@cp $(file < immuable.hvt.target) $@
	@chmod +w $@
	@echo " STRIP immuable.hvt"
	@strip $@

immuable.exe: immuable.exe.target
	@echo " COPY immuable.exe"
	@cp $(file < immuable.exe.target) $@

immuable.install: immuable.hvt immuable.exe
	@echo " GEN immuable.install"
	@ocaml install.ml > $@

all: immuable.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f immuable.hvt.target
	rm -f immuable.hvt
	rm -f immuable.exe.target
	rm -f immuable.exe
	rm -f immuable.install

install: immuable.intall
	@echo " INSTALL immuable"
	opam-installer immuable.install
