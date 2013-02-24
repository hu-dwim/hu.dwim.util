;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.util.production
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "Various utilities, contains code for producing standalone executable services."
  :depends-on (:cl-fad
               :command-line-arguments
               :hu.dwim.logger
               :hu.dwim.perec.postgresql     ; TODO drop dependency
               :hu.dwim.util.error-handling
               :hu.dwim.util.temporary-files
               :hu.dwim.util+iolib
               :hu.dwim.web-server.application ; TODO drop dependency
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "production")
                             (:file "command-line")))))
