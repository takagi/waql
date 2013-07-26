#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql-sandbox)

;;;
;;; User
;;;

(defstruct (user (:constructor user (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Action Event
;;;

(defstruct (action-event (:constructor action-event (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Action
;;;

(defstruct (action (:constructor action (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion Event
;;;

(defstruct (conversion-event (:constructor conversion-event (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion
;;;

(defstruct (conversion (:constructor conversion (id)))
  (id nil :type fixnum :read-only t))
