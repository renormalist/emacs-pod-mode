VERSION = `grep '^;;; Version:' pod-mode.el | cut -d: -f2 | sed 's/ //g'`
DISTNAME = pod-mode-$(VERSION)

all: dist

dist:
	mkdir $(DISTNAME)
	cp pod-mode.el README ChangeLog $(DISTNAME)/
	echo tar czf $(DISTNAME).tgz $(DISTNAME)
