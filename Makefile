VERSION := 5.1.0

all:
	@dune build
.PHONY: all

doc:
	@dune build @doc
.PHONY: doc

clean:
	@dune clean
.PHONY: clean

distclean: clean
	@find . -name "*~" -type f -exec rm {} \;
.PHONY: distclean

tests:
	@dune runtest
.PHONY: tests

bench: all
	@ulimit -s unlimited && perf stat _build/default/benchmark/church.exe  > /dev/null 2>  perf.log
	@ulimit -s unlimited && perf stat _build/default/benchmark/scott.exe   > /dev/null 2>> perf.log
	@ulimit -s unlimited && perf stat _build/default/benchmark/lambdas.exe > /dev/null 2>> perf.log
	@cat perf.log
	@rm perf.log
.PHONY: bench

promote:
	@dune promote
.PHONY: promote

install:
	@dune install
.PHONY: install

uninstall:
	@dune uninstall
.PHONY: uninstall

## Documentation webpage

updatedoc: doc
	@rm -rf docs/odoc
	@cp -r _build/default/_doc/_html docs/odoc
.PHONY: updatedoc

## Release

release: distclean
	git push origin
	git tag -a ocaml-bindlib_$(VERSION)
	git push origin ocaml-bindlib_$(VERSION)
.PHONY: release
