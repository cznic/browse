# Copyright 2016 The GC Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all clean cover cpu editor internalError later mem nuke todo edit fuzz fuzz2 fuzz-more

grep=--include=*.go --include=*.l --include=*.y --include=*.yy
ngrep='TODOOK\|.*_string\.go\|testdata/errchk'

all: editor
	go vet 2>&1 | grep -v $(ngrep) || true
	golint 2>&1 | grep -v $(ngrep) || true
	make todo
	unused . || true
	misspell *.go
	gosimple || true
	unconvert || true
	maligned || true

clean:
	go clean
	rm -f *~ *.test *.out gc-fuzz.zip y.output

cover:
	t=$(shell tempfile) ; go test -coverprofile $$t && go tool cover -html $$t && unlink $$t

cpu: clean
	go test -run @ -bench . -cpuprofile cpu.out
	go tool pprof -lines *.test cpu.out

declarationkind_string.go: enum.go
	stringer -type DeclarationKind

edit:
	@2>/dev/null gvim -p Makefile all_test.go log context.go decl.go enum.go \
		etc.go parser.go scanner.go type.go

editor: declarationkind_string.go
	@echo $(shell LC_TIME=c date) | tee log
	gofmt -l -s -w *.go
	go test 2>&1 | tee -a log
	#go build

fuzz:
	go-fuzz-build -func FuzzLexer github.com/cznic/gc
	rm -rf testdata/fuzz/lexer/corpus/ testdata/fuzz/lexer/crashers/ testdata/fuzz/lexer/suppressions/
	-go-fuzz -bin gc-fuzz.zip -workdir testdata/fuzz/lexer/

fuzz-more:
	-go-fuzz -bin gc-fuzz.zip -workdir testdata/fuzz/lexer/

fuzz2:
	cat $$(ls testdata/fuzz/lexer/crashers/*.output | head -n 1)

internalError:
	egrep -ho '"internal error.*"' *.go | sort | cat -n

later:
	@grep -n $(grep) LATER * || true
	@grep -n $(grep) MAYBE * || true

mem: clean
	go test -run @ -bench Parser/Std$$ -memprofile mem.out -memprofilerate 1 -timeout 24h
	go tool pprof -lines -web -alloc_space *.test mem.out

nuke: clean
	go clean -i

scopekind_string.go: enum.go
	stringer -type ScopeKind

todo:
	@grep -nr $(grep) ^[[:space:]]*_[[:space:]]*=[[:space:]][[:alpha:]][[:alnum:]]* * | grep -v $(ngrep) || true
	@grep -nr $(grep) TODO * | grep -v $(ngrep) || true
	@grep -nr $(grep) BUG * | grep -v $(ngrep) || true
	@grep -nr $(grep) [^[:alpha:]]println * | grep -v $(ngrep) || true
