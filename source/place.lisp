;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Place related

(def (macro e) notf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect `(not ,place)))))

(def (macro e) clearf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect nil))))
