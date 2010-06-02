VERSION = `grep '^;;; Version:' pod-mode.el | cut -d: -f2 | sed 's/ //g'`
DISTNAME = pod-mode-$(VERSION)

all:
	@echo Nothing to do. Try make dist.

meta:
	@V=$(VERSION) ; echo VERSION: $$V
	@V=$(VERSION) perl -pni -e 's/^(    |)version: \d+\.\d+/$$1version: $$ENV{V}/' META.yml
	@perl cpanmeta.pl

dist: meta
	@mkdir -p $(DISTNAME)
	@cp pod-mode.el README ChangeLog META.yml META.json $(DISTNAME)/
	@tar czf $(DISTNAME).tgz $(DISTNAME)
	@/bin/rm -fr $(DISTNAME)

clean:
	/bin/rm -f pod-mode-?.*.tgz
