; (depends-on "acl2s-interface.lsp")
; (depends-on "acl2s-interface-utils.lsp")
(in-package "ACL2S")
(include-book "tools/include-raw" :dir :system)
(include-book "xdoc/defxdoc-raw" :dir :system)
(set-ignore-ok t)
(make-event
 (er-progn (assign acl2s-result nil)
           (value '(value-triple (@ acl2s-result))))
 :check-expansion t)

(include-book "itest-ithm")
(defttag :acl2s-interface)
(acl2::include-raw "acl2s-interface.lsp")
(acl2::include-raw "acl2s-interface-utils.lsp")
#|
(xdoc::xdoc-extend acl2::acl2-sedan
                   "<p>See the @(see acl2s-interface) book for information on utilities for interfacing with ACL2s from Common Lisp.</p>")
|#
;; Uncomment the below block to generate documentation in this directory.
#|
;; this is required to run xdoc::save on versions of ACL2 before commit
;; b233d36b7225307d5a687c8294a48469631e97cc
;; doubly commented out to fool cert.pl dependency checker
;;(include-book "std/util/defval-tests" :dir :system)
(include-book "xdoc/save" :dir :system)
(xdoc::save "./doc" :error t :redef-okp t)
|#
