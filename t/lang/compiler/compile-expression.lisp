#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :waql-test.lang.compiler.compile-expression)

(plan nil)

;;
;; test COMPILE-LITERAL function
;;

(diag "COMPILE-LITERAL")

(is (compile-literal 1) 1
    "basic case 1")

(is (compile-literal "foo") "foo"
    "basic case 2")

(is (compile-literal '(time "2013-1-1" "00:00:00"))
    '(parse-timestring "2013-1-1T00:00:00")
    "basic case 3")

(is-error (compile-literal t) simple-error
          "EXPR which is an invalid expression.")


;;
;; test COMPILE-VARIABLE-REFERENCE function
;;

(diag "COMPILE-VARIABLE-REFERENCE")

(let ((compenv (add-letvar-compenv 'x :int 1
                 (empty-compenv))))
  (is (compile-variable-reference 'x compenv nil nil) 1
      "basic case"))

(is-error (compile-variable-reference 'x 'foo nil nil) type-error
          "COMPENV which is not a compiling environment")

(let ((compenv (empty-compenv)))
  (is-error (compile-variable-reference 'x compenv nil nil) simple-error
            "EXPR which is unbound"))

(let ((compenv (add-letfun-compenv 'f '((i :int)) :int 'i
                 (empty-compenv))))
  (is-error (compile-variable-reference 'f compenv nil nil) simple-error
            "EXPR which is bound to a function"))


;;
;; test COMPILE-LET function
;;

(diag "COMPILE-LET")

(let ((compenv (empty-compenv)))
  (is (compile-let '(let (x 1) x) compenv nil) 1
      "basic case 1"))

(let ((compenv (empty-compenv)))
  (is (compile-let '(let (f ((i :int)) i) (f 1)) compenv nil) 1
      "basic case 2"))


;;
;; test COMPILE-QUERY function
;;

(diag "COMPILE-QUERY")

(let* ((compenv (add-argvar-compenv 'r '(:relation :int :int) '+r+
                  (empty-compenv)))
       (lookup-keys (list (make-lookup-key '(1 nil) compenv nil))))
  (is (compile-query '(query (x y) (<- (x y) r)
                                   (= x y))
                     compenv 'foo lookup-keys)
      '(iterate:iter outermost
         (for-tuple (foo.x foo.y) in-relation +r+
                                  using (list (list 1 nil)))
         (when (= foo.x foo.y)
           (iterate:in outermost
             (collect-relation (tuple foo.x foo.y)))))
      "basic case"))


;;
;; test COMPILE-LISP-FORM function
;;

(diag "COMPILE-LISP-FORM")

(is (compile-lisp-form '(lisp foo :int)) 'foo
    "basic case")


;;
;; test COMPILE-FUNCTION function
;;

(diag "COMPILE-FUNCTION")

(let ((*scoping-count* 1)
      (compenv (add-letfun-compenv 'f '((i :int)) '(:relation :int)
                                   '(query (i) (<- (x) r))
                 (add-argvar-compenv 'r '(:relation :int) '+r+
                   (empty-compenv)))))
  (is (compile-function '(f 1) compenv nil)
      '(iterate:iter outermost
         (for-tuple (%f1.x) in-relation +r+)
         (iterate:in outermost
           (collect-relation (tuple 1))))
      "basic case 1"))

(let ((compenv (add-argvar-compenv 'r '(:relation :int) '+r+
                 (empty-compenv))))
  (is (compile-function '(count
                           (query (x) (<- (x) r))) compenv nil)
      '(relation-count
         (iterate:iter outermost
           (for-tuple (x) in-relation +r+)
           (iterate:in outermost
             (collect-relation (tuple x)))))
      "basic case 2"))


;;
;; test COMPUTE-LOOKUP-KEYS function
;;

(diag "PROPER-LOOKUP-KEYS")

(let ((%x1 (percent-symbol 'x1 1))
      (%x2 (percent-symbol 'x2 2))
      (%y1 (percent-symbol 'y 1))
      (%_1 (percent-symbol '_ 1))
      (compenv (add-letvar-compenv 'y :string "foo"
                 (add-letvar-compenv 'x2 :int 2
                   (add-letvar-compenv 'x1 :int 1
                     (empty-compenv))))))
  (let ((lookup-keys (proper-lookup-keys
                       `(,%x1 ,%x2 ,%y1 ,%_1 nil) compenv nil)))
    ;; test the first lookup-key
    (let ((lookup-key (first lookup-keys)))
      (is (lookup-key-elements lookup-key) '(x1 nil nil nil nil)
          "basic case 1")
      (is (lookup-key-compenv lookup-key) compenv
          "basic case 2")
      (is (lookup-key-scope lookup-key) nil
          "basic case 3"))
    ;; test the second lookup-key
    (let ((lookup-key (second lookup-keys)))
      (is (lookup-key-elements lookup-key) '(nil x2 nil nil nil)
          "basic case 4"))
    ;; test the third lookup-key
    (let ((lookup-key (third lookup-keys)))
      (is (lookup-key-elements lookup-key) '(nil nil y nil nil)
          "basic case 5"))
    ;; LOOKUP-KEYS has only three elements
    (is (length lookup-keys) 3
        "basic case 6")))

(diag "DERIVED-LOOKUP-KEYS")

(let* ((compenv (empty-compenv))
       (derived (list (make-lookup-key '(1 nil) compenv nil)
                      (make-lookup-key '(nil 1) compenv nil))))
  (let ((lookup-keys (derived-lookup-keys
                       derived '((let (x 1) x) x) '(x y))))
    ;; test the first lookup-key
    (let ((lookup-key (first lookup-keys)))
      (is (lookup-key-elements lookup-key) '(1 nil)
          "basic case 1")
      (is (lookup-key-compenv lookup-key) compenv
          "basic case 2")
      (is (lookup-key-scope lookup-key) nil
          "basic case 3"))
    ;; LOOKUP-KEYS has only one element
    (is (length lookup-keys) 1
        "basic case 4")))


;;
;; test COMPILE-LOOKUP-KEYS function
;;

(diag "COMPILE-LOOKUP-KEYS")

(let ((compenv1 (empty-compenv))
      (compenv2 (add-letvar-compenv 'x :int 1
                  (empty-compenv))))
  (let ((lookup-key1 (make-lookup-key '(1 nil) compenv1 nil))
        (lookup-key2 (make-lookup-key '(nil x) compenv2 'foo)))
    (let ((lookup-keys (list lookup-key1 lookup-key2)))
      (is (compile-lookup-keys lookup-keys)
          '(list (list 1 nil) (list nil 1))
          "basic case"))))

(is-error (compile-lookup-keys 'foo) type-error
          "LOOKUP-KEYS which is not a list")


;;
;; test MAKE-LOOKUP-KEY function
;;

(diag "MAKE-LOOKUP-KEY")

(let* ((compenv (add-letvar-compenv 'x :int 1
                  (empty-compenv)))
       (lookup-key (make-lookup-key '(x nil) compenv 'foo)))
  (is (lookup-key-elements lookup-key) '(x nil)
      "basic case 1")
  (is (lookup-key-compenv lookup-key) compenv
      "basic case 2")
  (is (lookup-key-scope lookup-key) 'foo
      "basic case 3"))

(let ((compenv (empty-compenv)))
  (is-error (make-lookup-key 'foo compenv nil) type-error
            "ELEMENTS which is not a list"))

(is-error (make-lookup-key '(x nil) 'foo nil) type-error
          "COMPENV which is not a compiling environment")

(let ((compenv (empty-compenv)))
  (is-error (make-lookup-key '(x nil) compenv 1) type-error
            "SCOPE which is not a WAQL symbol"))




(finalize)
