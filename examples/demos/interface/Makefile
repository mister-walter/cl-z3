all: cert

ACL2_CERT_PL ?= ${ACL2_SYSTEM_BOOKS}/build/cert.pl
ACL2_CLEAN_PL ?= ${ACL2_SYSTEM_BOOKS}/build/clean.pl

cert: top.lisp itest-ithm.lisp itest-cgen.lisp acl2s-interface.lsp acl2s-interface-utils.lsp cert.acl2
	$(ACL2_CERT_PL) top.lisp

clean:
	$(ACL2_CLEAN_PL) *.lisp

.PHONY: clean
