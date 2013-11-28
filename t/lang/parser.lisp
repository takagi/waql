#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %is-parsed (parser string expected complete-p args)
    (with-gensyms (result suffix success front)
      `(multiple-value-bind (,result ,suffix ,success ,front)
           (parse-string* ,parser ,string :complete ,complete-p)
         (declare (ignore ,suffix ,success ,front))
         (is ,result ,expected ,@args)))))

(defmacro is-parsed (parser string expected &rest args)
  (%is-parsed parser string expected t args))

(defmacro is-parsed-partially (parser string expected &rest args)
  (%is-parsed parser string expected nil args))


(plan nil)

;;
;; test Tokens
;;

(diag "Tokens")

(is-parsed (identifier*) "foo" "foo"
           "IDENTIFIER* 1")

(is-parsed (identifier*) "foo123" "foo123"
           "IDENTIFIER* 2")

(is-parsed (identifier*) "foo_123" "foo_123"
           "IDENTIFIER* 3")

(is-parsed (identifier*) "foo_" "foo_"
           "IDENTIFIER* 4")

(is-parsed (identifier*) "1foo" nil
           "IDENTIFIER* 5")

(is-parsed (identifier*) "+foo+" "+foo+"
           "IDENTIFIER* 6")

(is-parsed (identifier*) "+foo" nil
           "IDENTIFIER* 7")

(is-parsed (is-pure-word* "foo") "foo" "foo"
           "IS-PURE-WORD* 1")

(is-parsed-partially (is-pure-word* "foo") "foo123" "foo"
                     "IS-PURE-WORD* 2")

(is-parsed-partially (is-pure-word* "foo") "foobar" nil
                     "TO FAIL: IS-PURE-WORD* 3")


;;
;; test Comment and Whitestuff
;;

(diag "Comment and Whitestuff")

(is-parsed (whitestuffs*) "  " nil
           "WHITESTUFFS* 1")

(is-parsed (whitestuff*) "-- foo" "-- foo"
           "WHITESTUFF* 1")

(is-parsed (whitestuff*) " " nil
           "WHITESTUFF* 2")

(is-parsed (whitestuff*) (string #\Newline) nil
           "WHITESTUFF* 3")

(is-parsed (whitestuff*) (string #\Tab) nil
           "WHITESTUFF* 4")

(is-parsed (seq-list* (~ws* "foo")
                      (~ws* "bar"))
           "foo bar" (list "foo" "bar")
           "~WS* 1")


;;
;; test Basic elements
;;

(diag "Basic elements")

(is-parsed (underscore*) "_ " '_
           "UNDERSCORE* 1")

(is-parsed (waql-symbol*) "foo " 'foo
           "WAQL-SYMBOL 1")

(is-parsed (waql-symbol*) "foo123 " 'foo123
           "WAQL-SYMBOL 2")

(is-parsed (waql-symbol*) "foo_123 " 'foo-123
           "WAQL-SYMBOL 3")

(is-parsed (waql-symbol*) "foo_ " 'foo-
           "WAQL-SYMBOL 4")

(is-parsed (waql-symbol*) "1foo " nil
           "WAQL-SYMBOL 5")

(is-parsed (waql-symbol*) "+foo+ " '+foo+
           "WAQL-SYMBOL 6")

(is-parsed (waql-symbol*) "+foo " nil
           "WAQL-SYMBOL 7")

(is-parsed (tuple* (waql-symbol*)) "< foo , bar , baz >"
           '(foo bar baz)
           "TUPLE* 1")


;;
;; test WAQL expression
;;

(diag "WAQL expression")

(is-parsed (expr*) "let x := 1
                    in -- x is bound to 1
                       x"
           '(let (x 1) x)
           "EXPR* 1")

(is-parsed (expr*) "let x := time 2013-1-1 10:10:35
                    in x + days 30"
           '(let (x (time "2013-1-1" "10:10:35"))
              (+ x (days 30)))
           "EXPR* 2")

(is-parsed (expr*) "let" nil
           "EXPR* 3")

(is-parsed (expr-top*) "123;" 123
           "EXPR-TOP* 1")

(is-parsed (expr-top*) "    1;" 1
           "EXPR-TOP* 2")

(is-parsed (expr-top*) "    ;" nil
           "EXPR-TOP* 3")

(is-parsed (expr-top*) " -- foo;
123;" 123
           "EXPR-TOP* 4")

(is-parsed (expr-top*) "123;123;" nil
           "EXPR-TOP* 5")

(is-parsed (expr-top*) "" nil
           "EXPR-TOP* 6")

(is-parsed (expr-top*) " " nil
           "EXPR-TOP* 7 - a whitestuff")

(is-parsed (expr-top*) " -- foo" nil
           "EXPR-TOP* 8 - two whitestuffs")

(is-parsed (expr-top*) "  ;" nil
           "EXPR-TOP* 9 - two whitestuffs and a semicolon")

(is-parsed (expr-top*) ";" nil
           "EXPR-TOP* 10 - a semicolon")

(is-parsed (expr-top*) " -- foo;
;" nil
           "EXPR-TOP* 11 - two whitestuffs and a semicolon")


;;
;; test Literal
;;

(diag "Literal")

(is-parsed (literal*) "123" 123
           "LITERAL* 1")

(is-parsed (literal*) "\"foo\"" "foo"
           "LITERAL* 2")

(is-parsed (literal*) "time 2013-1-1 00:00:00"
           (make-time-literal "2013-1-1" "00:00:00")
           "LITERAL* 3")


;;
;; test Let
;;

(diag "Let")

(is-parsed (let-*) "let x := 1 in x" '(let (x 1) x)
           "LET 1")

(is-parsed (let-*) "let x := y in x" '(let (x y) x)
           "LET 2")

(is-parsed (let-*) "let f i:int := i
                    in f 1"
           '(let (f ((i :int)) i) (f 1))
           "LET 3")

(is-parsed (let-*) "let f i := i
                    in f 1"
           nil
           "LET 4")


;;
;; test Query
;;

(diag "Query")

(is-parsed (query*) "{ <a, b> | <a, b> <- +r+
                              , a = 1 }"
           '(query (a b) (<- (a b) +r+)
                         (= a 1))
           "QUERY 1")

(is-parsed (query*) "{ <a> | <a, _> <- +r+ }"
           '(query (a) (<- (a _) +r+))
           "QUERY 2")


;;
;; test Function application
;;

(diag "Function application")

(is-parsed (function*) "f i j" '(f i j)
           "FUNCTION* 1")

(is-parsed (function*) "f f i" '(f f i)
           "FUNCTION* 2")

(is-parsed (function*) "f (f i)" '(f (f i))
           "FUNCTION* 3")

(is-parsed (function*) "1 = 1" '(= 1 1)
           "FUNCTION* 4")

(is-parsed (function*) "1 = f i = 1" '(= (= 1 (f i)) 1)
           "FUNCTION* 5")


;;
;; test WAQL types
;;

(diag "WAQL types")

(is-parsed (waql-type*) "bool" :bool
           "WAQL-TYPE* 1")

(is-parsed (waql-type*) "int" :int
           "WAQL-TYPE* 2")

(is-parsed (waql-type*) "string" :string
           "WAQL-TYPE* 3")

(is-parsed (waql-type*) "time" :time
           "WAQL-TYPE* 4")

(is-parsed (waql-type*) "interval" :interval
           "WAQL-TYPE* 5")

(is-parsed (waql-type*) "{<int,int>}" '(:relation :int :int)
           "WAQL-TYPE* 6")


(finalize)
