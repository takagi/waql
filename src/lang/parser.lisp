#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql.lang.parser)

;;
;;  Syntax:
;;
;;    PARSE-WAQL string &key top-expr-p => expr
;;
;;  Arguments and Values:
;;
;;    string --- a string
;;    top-expr-p --- a generalised boolean. The default is t
;;    expr --- a WAQL expression
;;
;;  Description:
;;
;;    None.
;;
;;  Exceptional Situation:
;;
;;    Signals an error of type type-error if STRING is not a string.
;;
(define-condition waql-parse-error (error)
  ((string :reader waql-parse-error-string
           :initarg :string
           :type string))
  (:report waql-parse-error-printer))

(defun waql-parse-error-printer (condition stream)
  (let ((string (waql-parse-error-string condition)))
    (format stream "Parse error: ~A" string)))

(defun parse-waql (string)
  (multiple-value-bind (result suffix success front)
      (parse-string* (expr-top*) string :complete t)
    (declare (ignore suffix))
    (unless success
      (let ((front-string (front-string front string)))
        (error 'waql-parse-error :string front-string)))
    result))

(defparameter +front-string-margin+ 0)

(defun front-string (front string)
  (let ((pos (position-of front)))
    (if (<= pos +front-string-margin+)
        string
        (concatenate 'string "..."
          (subseq string (- pos +front-string-margin+))))))

(defun parse-blank-line (string)
  (multiple-value-bind (result suffix success front)
      (parse-string* (blank-line*) string :complete t)
    (declare (ignore result suffix front))
    success))


;;
;; Parser Combinators - Tokens
;;

(defun graphic-char? ()
  (sat #'graphic-char-p))

(defun printable-char? ()
  (except? (graphic-char?) #\Space))

(defun identifier* ()
  (choice1 (plus-enclosed-identifier*)
           (ordinal-identifier*)))

(defun plus-enclosed-identifier* ()
  (named-seq* #\+
              (<- string (ordinal-identifier*))
              #\+
              (format nil "+~A+" string)))

(defun ordinal-identifier* ()
  (labels ((identifier-head* ()
             (letter?))
           (identifier-tail* ()
             (between* (choice1 (alphanum?) #\_) nil nil 'string)))
    (named-seq* (<- head (identifier-head*))
                (<- tail (identifier-tail*))
                (format nil "~A~A" head tail))))

(defun is-pure-word* (expected)
  expected)


;;
;; Parser Combinators - Comment and Whitestuff
;;

(defun comment* ()
  (named-seq* "--"
              (<- any (between? (graphic-char?) nil nil 'string))
              (format nil "--~A" any)))

(defun whitestuff* ()
  (choice1 (whitespace?)
           (comment*)))

(defun ~ws* (Q)
  ;; skip whitespaces and comments following Q
  (named-seq* (<- result Q)
              (many* (whitestuff*))
              result))


;;
;; Parser Combinators - Basic elements
;;

(defun underscore* ()
  (~ws* (named-seq* #\_ (symbolicate "_"))))

(defun waql-symbol* ()
  (~ws* (except? (named-seq* (<- string (identifier*))
                             (symbolicate
                               (substitute #\- #\_ 
                                 (string-upcase string))))
                 (reserved*))))

(defun reserved* ()
  (choices1 "let" "in" "time" "bool" "int" "string" "interval"))

(defun tuple* (Q)
  (bracket? (~ws* #\<)
            (sepby1* Q (~ws* #\,))
            (~ws* #\>)))


;;
;; Parser Combinators - WAQL expression
;;

(defun expr-top* ()
  (named-seq* (many* (whitestuff*))
              (<- result (expr*))
              (~ws* #\;)
              result))

(defun expr* ()
  (choices1 (let-*)
            (query*)
            (function*)
            (literal*)
            (variable-reference*)))

(defun blank-line* ()
  (choice1 (comment*)
           (~ws* #\;)))


;;
;; Parser Combinators - Literal
;;

(defun literal* ()
  (choices1 (int-literal*)
            (string-literal*)
            (time-literal*)))

(defun int-literal* ()
  (~ws* (int*)))

(defun string-literal* ()
  (~ws* (quoted?)))

(defun time-literal* ()
  (named-seq* (~ws* (is-pure-word* "time"))
              (<- date (time-literal-date*))
              (<- time (time-literal-time*))
              (make-time-literal date time)))

(defun time-literal-date* ()
  (~ws* (between? (printable-char?) 1 nil 'string)))

(defun time-literal-time* ()
  (~ws* (between? (printable-char?) 1 nil 'string)))


;;
;; Parser Combinators - Variable reference
;;

(defun variable-reference* ()
  (waql-symbol*))


;;
;; Parser Combinators - Let
;;

(defun let-* ()
  (choice1 (let-variable*)
           (let-function*)))

(defun let-variable* ()
  (named-seq* (~ws* (is-pure-word* "let"))
              (<- var (let-var*))
              (~ws* ":=")
              (<- local-expr (let-local-expr*))
              (~ws* (is-pure-word* "in"))
              (<- body-expr (let-body-expr*))
              (make-let-variable var local-expr body-expr)))

(defun let-function* ()
  (named-seq* (~ws* (is-pure-word* "let"))
              (<- var (let-var*))
              (<- args (let-arguments*))
              (~ws* ":=")
              (<- local-expr (let-local-expr*))
              (~ws* (is-pure-word* "in"))
              (<- body-expr (let-body-expr*))
              (make-let-function var args local-expr body-expr)))

(defun let-var* ()
  (waql-symbol*))

(defun let-arguments* ()
  (many1* (argument*)))

(defun let-local-expr* ()
  (delayed? (expr*)))

(defun let-body-expr* ()
  (delayed? (expr*)))

(defun argument* ()
  (named-seq* (<- var (waql-symbol*))
              (~ws* ":")
              (<- type (waql-type*))
              (make-argument var type)))


;;
;; Parser Combinators - Query
;;

(defun query* ()
  (named-seq* (~ws* "{")
              (<- exprs (query-exprs*))
              (~ws* "|")
              (<- quals (query-quals*))
              (~ws* "}")
              (make-query exprs quals)))

(defun query-quals* ()
  (sepby1* (query-qual*) (~ws* #\,)))

(defun query-qual* ()
  (choice1 (quantification*)
           (predicate*)))

(defun quantification* ()
  (named-seq* (<- vars (quantification-vars*))
              (~ws* "<-")
              (<- relation (quantification-relation*))
              (make-quantification vars relation)))

(defun quantification-vars* ()
  (tuple* (choice1 (waql-symbol*)
                   (underscore*))))

(defun quantification-relation* ()
  (delayed? (expr*)))

(defun predicate* ()
  (delayed? (expr*)))

(defun query-exprs* ()
  (tuple* (delayed? (expr*))))


;;
;; Parser Combinators - Function application
;;

(defun function* ()
  (choice1 (infix-function*)
           (prefix-function*)))

(defun infix-function* ()
  (expression* (infix-aexpr*)
               `((,(additive-op*) :left)
                 (,(comparison-op*) :left))))

(def-cached-parser additive-op*
  (choice1 (mdo (~ws* #\+) (result (curry #'list '+)))
           (mdo (~ws* #\-) (result (curry #'list '-)))))

(def-cached-parser comparison-op*
  (choices1 (mdo (~ws* ">=") (result (curry #'list '>=)))
            (mdo (~ws* #\>)  (result (curry #'list '>)))
            (mdo (~ws* "<=") (result (curry #'list '<=)))
            (mdo (~ws* #\<)  (result (curry #'list '<)))
            (mdo (~ws* #\=)  (result (curry #'list '=)))))

(defun infix-aexpr* ()
  (choices1 (enclosed-expr*)
            (literal*)
            (let-*)
            (query*)
            (prefix-function*)))

(defun prefix-function* ()
  (named-seq* (<- func (waql-symbol*))
              (<- aexprs (many* (prefix-aexpr*)))
              (if (null aexprs)
                  func
                  (make-function func aexprs))))

(defun prefix-aexpr* ()
  (choices1 (enclosed-expr*)
            (literal*)
            (let-*)
            (query*)
            (variable-reference*)))

(defun enclosed-expr* ()
  (bracket? (~ws* #\()
            (delayed? (expr*))
            (~ws* #\))))


;;
;; Parser Combinators - WAQL types
;;

(defun waql-type* ()
  (choices1 (ty-scalar*)
            (ty-relation*)
            (ty-function*)))

(defun ty-scalar* ()
  (choices1 (ty-interval*)
            (ty-bool*)
            (ty-int*)
            (ty-string*)
            (ty-time*)))
            
(defun ty-bool* ()
  (named-seq* (~ws* (is-pure-word* "bool"))
              :bool))

(defun ty-int* ()
  (named-seq* (~ws* (is-pure-word* "int"))
              :int))

(defun ty-string* ()
  (named-seq* (~ws* (is-pure-word* "string"))
              :string))

(defun ty-time* ()
  (named-seq* (~ws* (is-pure-word* "time"))
              :time))

(defun ty-interval* ()
  (named-seq* (~ws* (is-pure-word* "interval"))
              :interval))

(defun ty-relation* ()
  (named-seq* (<- attr-types
                  (bracket? (~ws* #\{)
                            (tuple* (delayed? (waql-type*)))
                            (~ws* #\})))
              (make-relation-type attr-types)))

(defun ty-function* ()
  (zero))                               ; no function type syntax
