;;;; File:  load.scm -- Loader for pattern matching system

; Pattern matcher:

(load "eq-properties")
(load "ghelper")
(load "matcher")


; Term rewriting / pattern-directed invocation system:

(define (rule-memoize f) f) ; A stub put in place in case you want to
							; play with memoization in the term
							; rewriter
(load "utils.scm")
(load "pattern-directed-invocation")
(load "rules")
