#|
  This file is a part of waql project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :waql)

;;;
;;; User
;;;

(defstruct (user (:constructor user (id)))
  (id nil :type fixnum :read-only t))

(defun user= (user1 user2)
  (= (user-id user1) (user-id user2)))


;;;
;;; Event
;;;

(defstruct (event (:constructor event (id)))
  (id nil :type fixnum :read-only t))

(defun event< (event1 event2)
  (< (event-id event1) (event-id event2)))


;;;
;;; Action
;;;

(defstruct (action (:constructor action (id)))
  (id nil :type fixnum :read-only t))


;;;
;;; Conversion
;;;

(defstruct (conversion (:constructor conversion (id)))
  (id nil :type fixnum :read-only t))

