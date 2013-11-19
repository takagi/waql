#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :cl-user)

(defpackage waql-test.util
  (:use :cl
        :cl-test-more
        :waql.util))

(defpackage waql-test.util.coroutine
  (:use :cl
        :cl-test-more
        :waql.util.coroutine))

(defpackage waql-test.lang.data
  (:use :cl
        :cl-test-more
        :waql.lang.data)
  (:import-from #:waql.lang.data
                ;; WAQL-SYMBOL
                #:*scoping-count*
                ;; INTERVAL
                #:interval-unit-p
                #:interval-amount
                #:interval-unit
                ;; RELATION-INDEX
                #:empty-relation-index
                #:add-relation-index
                #:lookup-relation-index))

(defpackage waql-test.lang.type
  (:use :cl
        :cl-test-more
        :waql.lang.type))

(defpackage waql-test.lang.syntax
  (:use :cl
        :cl-test-more
        :waql.lang.syntax))

(defpackage waql-test.lang.compiler.patenv
  (:use :cl
        :cl-test-more
        :waql.lang.compiler.patenv))

(defpackage waql-test.lang.compiler.typenv
  (:use :cl
        :cl-test-more
        :waql.lang.compiler.typenv)
  (:import-from #:waql.lang.compiler.typenv
                #:add-typenv))

(defpackage waql-test.lang.compiler.compenv
  (:use :cl
        :cl-test-more
        :waql.lang.compiler.typenv
        :waql.lang.compiler.compenv))

(defpackage waql-test.lang.compiler.predefined-relations
  (:use :cl
        :cl-test-more
        :waql.lang.data
        :waql.lang.compiler.predefined-relations)
  (:import-from #:waql.lang.compiler.predefined-relations
                #:empty-predefined-relations
                #:add-predefined-relation))

(defpackage waql-test.lang.compiler.generic-functions
  (:use :cl
        :cl-test-more
        :waql.lang.compiler.generic-functions)
  (:import-from #:waql.lang.compiler.generic-functions
                #:make-specialized-function
                #:match-type-pattern))

(defpackage waql-test.lang.compiler.type-of
  (:use :cl
        :cl-test-more
        :waql.lang.syntax
        :waql.lang.compiler.typenv
        :waql.lang.compiler.type-of)
  (:import-from #:waql.lang.compiler.type-of
                #:type-of-literal
                #:type-of-variable-reference
                #:type-of-let
                #:type-of-query
                #:type-of-lisp-form
                #:type-of-function))

(defpackage waql-test.lang.compiler.pattern-match
  (:use :cl
        :cl-test-more
        :waql.lang.syntax
        :waql.lang.compiler.patenv
        :waql.lang.compiler.pattern-match)
  (:import-from #:waql.lang.compiler.pattern-match
                #:pattern-match-literal
                #:pattern-match-variable-reference
                #:pattern-match-let
                #:pattern-match-query
                #:pattern-match-lisp-form
                #:pattern-match-function
                #:run-pattern-matcher
                #:*underscore-count*))

(defpackage waql-test.lang.compiler.compile-expression
  (:use :cl
        :cl-test-more
        :waql.lang.data
        :waql.lang.syntax
        :waql.lang.compiler.predefined-relations
        :waql.lang.compiler.compenv
        :waql.lang.compiler.compile-expression)
  (:import-from #:waql.lang.compiler.compile-expression
                ;; COMPILE-EXPRESSION
                #:compile-literal
                #:compile-variable-reference
                #:compile-let
                #:compile-query
                #:outermost
                #:compile-lisp-form
                #:compile-function
                ;; COMPUTE-LOOKUP-KEYS
                #:proper-lookup-keys
                #:derived-lookup-keys
                #:compile-lookup-keys
                #:make-lookup-key
                #:lookup-key-elements
                #:lookup-key-compenv
                #:lookup-key-scope)
  (:import-from #:waql.lang.data
                #:*scoping-count*)
  (:import-from #:local-time
                #:parse-timestring))

(defpackage waql-test.lang.compiler
  (:use :cl
        :cl-test-more
        :waql.lang.data
        :waql.lang.syntax
        :waql.lang.compiler.predefined-relations
        :waql.lang.compiler)
  (:import-from #:waql.lang.data
                #:*scoping-count*)
  (:import-from #:waql.lang.compiler.compile-expression
                #:outermost))

(defpackage waql-test.lang.parser
  (:use :cl
        :cl-test-more
        :waql.lang.syntax
        :waql.lang.parser)
  (:import-from #:waql.lang.parser
                ;; Tokens
                #:identifier*
                #:is-pure-word*
                ;; Comment and Whitestuff
                #:whitestuff*
                #:~ws*
                ;; Baisc elements
                #:underscore*
                #:waql-symbol*
                #:tuple*
                ;; WAQL expression
                #:expr-top*
                #:expr*
                ;; Literal
                #:literal*
                ;; Let
                #:let-*
                ;; Query
                #:query*
                ;; Function application
                #:function*
                ;; WAQL types
                #:waql-type*)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:parser-combinators
                #:parse-string*
                #:seq-list*))

(defpackage waql-test.lang
  (:use :cl
        :cl-test-more
        :waql.lang.data
        :waql.lang.compiler.predefined-relations
        :waql.lang))

(defpackage waql-test
  (:use :cl
        :cl-test-more
        :waql))
