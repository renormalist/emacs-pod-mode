VERSION = `grep '^;;; Version:' pod-mode.el | cut -d: -f2 | sed 's/ //g'`
DISTNAME = pod-mode-$(VERSION)

all:
	@echo Nothing to do. Try make dist.

dist:
	@mkdir $(DISTNAME)
	@cp pod-mode.el README ChangeLog $(DISTNAME)/
	@tar czf $(DISTNAME).tgz $(DISTNAME)
	@/bin/rm -fr $(DISTNAME)

clean:
	/bin/rm -f pod-mode-?.?.tgz
