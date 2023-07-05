# Based on the Makefile for piem
# <https://git.kyleam.com/piem/tree/Makefile>
.POSIX:
.SUFFIXES: .el .elc .texi .info .html

EMACS = emacs
MAKEINFO = makeinfo

BATCH = $(EMACS) --batch -Q -L .

EL = git-email.el git-email-gnus.el git-email-notmuch.el git-email-magit.el \
     git-email-piem.el
ELC = $(EL:.el=.elc)

all: compile docs

compile: $(ELC)

clean:
	rm -rf doc/git-email.info $(ELC)
	rm -rf doc/html/

docs: doc/git-email.html doc/git-email.info
	rm -rf doc/html/
	$(MAKEINFO) --html -o doc/html/ doc/git-email.texi

git-email-gnus.elc: git-email-gnus.el git-email.elc
git-email-notmuch.elc: git-email-notmuch.el git-email.elc
git-email-magit.elc: git-email-magit.el git-email.elc
git-email-piem.elc: git-email-piem.el git-email.elc
git-email.elc: git-email.el

.el.elc:
	$(BATCH) -f batch-byte-compile $<

.texi.info:
	$(MAKEINFO) -o doc/ $<

.texi.html:
	$(MAKEINFO) --html -o doc/html/ -c TOP_NODE_UP_URL=/ --no-split $<
