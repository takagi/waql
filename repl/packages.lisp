#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#


(in-package :cl-user)

(defpackage waql-repl.repl-server
  (:use :cl :waql)
  (:export #:repl-server
           #:+quit-command-regexp+)
  (:import-from #:alexandria
                #:starts-with)
  (:import-from #:cl-coroutine
                #:defcoroutine
                #:yield
                #:coexit)
  (:import-from #:cl-ppcre
                #:scan
                #:register-groups-bind))

(defpackage waql-repl.cl-repl
  (:use :cl
        :waql-repl.repl-server)
  (:export repl-waql)
  (:import-from #:cl-coroutine
                #:with-coroutine))

(defpackage waql-repl.web-repl
  (:use :cl
        :waql-repl.repl-server)
  (:export #:start
           #:stop)
  (:import-from #:cl-coroutine
                #:make-coroutine)
  (:import-from #:cl-ppcre
                #:scan))

(defpackage waql-repl
  (:use :cl
        :waql-repl.cl-repl
        :waql-repl.web-repl)
  (:export #:repl-waql
           #:start
           #:stop))
