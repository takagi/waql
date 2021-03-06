#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :cl-user)

(defpackage waql.util
  (:use :cl)
  (:export #:single
           #:minimize
           #:flip
           #:trim
           #:left-trim
           #:right-trim
           #:semicolon-terminated
           #:semicolon-terminated-p
           #:ensure-semicolon-terminated
           #:maptree)
  (:import-from #:alexandria
                #:ends-with))

(defpackage waql.lang.data
  (:use :cl
        :waql.util)
  (:export ;; WAQL-SYMBOL
           #:waql-symbol-p
           #:percent-symbol-p
           #:percent-symbol
           #:original-symbol
           #:underscore-notation-p
           #:scoped-symbol
           #:scoping-symbol
           ;; WAQL-BOOLEAN
           #:waql-boolean-p
           ;; WAQL-INTEGER
           #:waql-integer-p
           ;; WAQL-STRING
           #:waql-string-p
           ;; TIME
           #:time+
           #:time-
           ;; INTERVAL
           #:minutes
           #:hours
           #:days
           #:weeks
           #:months
           #:years
           ;; TUPLE
           #:tuple
           #:tuple-ref
           #:tuple-dim
           #:print-tuple
           ;; RELATION
           #:empty-relation
           #:relation-p
           #:relation->list
           #:relation-count
           #:relation-exists
           #:relation-member
           #:relation-adjoin
           #:relation-adjoin-all
           #:relation-index-lookup
           #:for-tuple #:in-relation #:using
           #:collect-relation)
  (:import-from #:alexandria
                #:format-symbol
                #:hash-table-keys
                #:with-gensyms)
  (:import-from #:local-time
                #:timestamp+
                #:timestamp-))

(defpackage waql.lang.type
  (:use :cl)
  (:export ;; WAQL TYPE
           #:waql-type-p
           ;; SCALAR TYPE
           #:scalar-type-p
           ;; RELATION TYPE
           #:make-relation-type
           #:relation-type-p
           #:relation-type-attributes
           #:relation-type-dimension
           ;; FUNCTION TYPE
           #:make-function-type
           #:function-type-p
           #:function-type-argument-types
           #:function-type-return-type
           ))

(defpackage waql.lang.syntax
  (:use :cl
        :waql.lang.data
        :waql.lang.type)
  (:export ;; <literal>
           #:literal-p
           #:int-literal-p
           #:string-literal-p
           #:make-time-literal
           #:time-literal-p
           #:time-literal-date
           #:time-literal-time
           ;; <variable-reference>
           #:variable-reference-p
           ;; <let>
           #:make-let-variable
           #:make-let-function
           #:let-p
           #:let-variable-p
           #:let-function-p
           #:let-var
           #:let-args
           #:let-arg-vars
           #:let-arg-types
           #:let-local-expr
           #:let-body-expr
           ;; <argument>
           #:make-argument
           #:argument-p
           #:argument-var
           #:argument-type
           ;; <query>
           #:query #:<-
           #:make-query
           #:query-p
           #:query-exprs
           #:query-quals
           ;; <quantification>
           #:make-quantification
           #:quantification-p
           #:quantification-vars
           #:quantification-relation
           ;; <lisp-form>
           #:lisp
           #:make-lisp-form
           #:lisp-form-p
           #:lisp-form
           #:lisp-form-type
           ;; <function-application>
           #:make-function
           #:function-p
           #:function-operator
           #:function-operands))

(defpackage waql.lang.compiler.patenv
  (:use :cl
        :waql.lang.data)
  (:export #:empty-patenv
           #:patenv-p
           #:add-patenv
           #:bulk-add-patenv
           #:inc-patenv
           #:lookup-patenv))

(defpackage waql.lang.compiler.typenv
  (:use :cl
        :waql.lang.data
        :waql.lang.type
        :waql.lang.syntax)
  (:export #:empty-typenv
           #:typenv-p
           #:add-qvar-typenv
           #:add-argvar-typenv
           #:add-letvar-typenv
           #:add-letfun-typenv
           #:add-qvars-typenv
           #:add-argvars-typenv
           #:lookup-typenv))

(defpackage waql.lang.compiler.compenv
  (:use :cl
        :waql.lang.data
        :waql.lang.type
        :waql.lang.compiler.typenv)
  (:export #:empty-compenv
           #:compenv-p
           #:add-qvar-compenv
           #:add-argvar-compenv
           #:add-letvar-compenv
           #:add-letfun-compenv
           #:add-qvars-compenv
           #:add-argvars-compenv
           #:lookup-compenv
           #:compenv->typenv)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:waql.lang.syntax
                #:argument-p
                #:argument-var
                #:argument-type))

(defpackage waql.lang.compiler.defrelation
  (:use :cl
        :waql.lang.data
        :waql.lang.type)
  (:export #:defrelation
           #:clear-defrelation
           #:predefined-relations))

(defpackage waql.lang.compiler.generic-functions
  (:use :cl
        :waql.lang.data
        :waql.lang.type)
  (:export #:generic-function-p
           #:exists
           #:days
           #:specialized-function
           #:specialized-function-name
           #:specialized-function-generic-name
           #:specialized-function-argument-types
           #:specialized-function-return-type)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:local-time
                #:timestamp=
                #:timestamp<
                #:timestamp<=
                #:timestamp>
                #:timestamp>=))

(defpackage waql.lang.compiler.type-of
  (:use :cl
        :waql.lang.data
        :waql.lang.type
        :waql.lang.syntax
        :waql.lang.compiler.defrelation
        :waql.lang.compiler.generic-functions
        :waql.lang.compiler.typenv)
  (:export #:type-of-expression-top
           #:type-of-expression)
  (:import-from #:alexandria
                #:length=))

(defpackage waql.lang.compiler.pattern-match
  (:use :cl
        :waql.lang.data
        :waql.lang.syntax
        :waql.lang.compiler.defrelation
        :waql.lang.compiler.patenv)
  (:export #:pattern-match-expression-top
           #:pattern-match-expression)
  (:import-from #:alexandria
                #:with-gensyms))

(defpackage waql.lang.compiler.compile-expression
  (:use :cl
        :waql.lang.data
        :waql.lang.type
        :waql.lang.syntax
        :waql.lang.compiler.defrelation
        :waql.lang.compiler.generic-functions
        :waql.lang.compiler.typenv
        :waql.lang.compiler.compenv
        :waql.lang.compiler.type-of)
  (:export #:compile-expression-top
           #:compile-expression)
  (:import-from #:local-time
                #:parse-timestring)
  (:import-from #:alexandria
                #:iota))

(defpackage waql.lang.compiler
  (:use :cl
        :waql.lang.compiler.pattern-match
        :waql.lang.compiler.compile-expression)
  (:export #:compile-waql))

(defpackage waql.lang.parser
  (:use :cl
        #:waql.lang.syntax
        #:waql.lang.type
        #:parser-combinators)
  (:export #:parse-waql
           #:parse-blank-line
           #:waql-parse-error)
  (:import-from #:alexandria
                #:symbolicate
                #:compose
                #:curry)
  (:shadowing-import-from #:parser-combinators
                          #:<-))

(defpackage waql.lang
  (:use :cl
        :waql.util
        :waql.lang.compiler
        :waql.lang.parser)
  (:export #:waql
           #:waql-in-sexp
           #:eval-waql
           #:eval-waql-in-sexp
           #:precompile-waql
           #:precompile-waql-in-sexp))

(defpackage waql
  (:use :cl
        :waql.lang.data
        :waql.lang.syntax
        :waql.lang.compiler.generic-functions
        :waql.lang.compiler.defrelation
        :waql.lang)
  (:export ;; WAQL INTERFACES
           #:waql
           #:waql-in-sexp
           #:eval-waql
           #:eval-waql-in-sexp
           #:precompile-waql
           #:precompile-waql-in-sexp
           ;; DEFRELATION
           #:defrelation
           #:clear-defrelation
           ;; TUPLE
           #:tuple
           #:tuple-ref
           #:tuple-dim
           #:print-tuple
           ;; RELATION
           #:empty-relation
           #:relation-p
           #:relation->list
           #:relation-count
           #:relation-exists
           #:relation-member
           #:relation-adjoin
           #:relation-adjoin-all
           ;; SYNTAX
           #:query #:<-
           #:lisp
           ;; GENERIC FUNCTIONS
           #:exists
           #:days))

(defpackage waql.sandbox
  (:use :cl
        :waql)
  (:export #:+u+
           #:+ev+
           #:+cv+
           #:+ad+
           #:+eua+
           #:+euc+
           #:+uf1+)
  (:import-from #:local-time
                #:parse-timestring)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:split-sequence
                #:split-sequence))
