;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; human readable thread id's

(def (namespace :weakness :key) thread-id)
(def global-variable *thread-id-counter* (make-atomic-counter))

(def (function e) human-readable-thread-id (&optional (thread (bordeaux-threads:current-thread)))
  (assert (bordeaux-threads:threadp thread))
  (find-thread-id thread :otherwise (lambda ()
                                      (setf (find-thread-id thread)
                                            (atomic-counter/increment *thread-id-counter*)))))
