;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(unless (or #+asdf3 (uiop:version<= "2.31.1" (asdf-version)))
  (error "You need ASDF >= 2.31.1 to load this system correctly."))

(defsystem :hu.dwim.util
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar
               ;; TODO at the time of writing the sb-sprof contrib is broken on windows.
               ;; see https://bugs.launchpad.net/sbcl/+bug/1274943
               ;; see also below
               #+(and sbcl (not windows)) :sb-sprof)
  :components ((:module "source"
                :components ((:file "anaphora" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("miscellaneous"))
                             (:file "error-handling-early" :depends-on ("package" "miscellaneous"))
                             (:file "generic-operator" :depends-on ("package"))
                             (:file "hash-table" :depends-on ("package"))
                             (:file "integer-to-string" :depends-on ("package"))
                             (:file "iterate" :depends-on ("package"))
                             (:file "number" :depends-on ("package"))
                             (:file "package")
                             (:file "pattern-matcher" :depends-on ("package"))
                             (:file "place" :depends-on ("package"))
                             (:file "sequence" :depends-on ("package"))
                             (:file "string" :depends-on ("miscellaneous"))
                             (:file "string-early" :depends-on ("package"))
                             (:file "threads-early" :depends-on ("package"))
                             (:file "type" :depends-on ("package"))
                             (:file "miscellaneous" :depends-on ("package" "string-early"))))
                (:module "integration"
                 :components (#+(and sbcl (not windows)) (:file "sbcl")))))
