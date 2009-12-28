;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Definition source text database

(def special-variable *definition-source-texts* (make-hash-table :synchronized #t))

(def function clear-definition-source-texts ()
  (clrhash *definition-source-texts*))

(def function read-definition-source-text (definition)
  ;; KLUDGE: all this hassle is to workaround SBCL's bootstrapping package names, this is obviously non thread safe
  (bind ((original-find-package (fdefinition 'find-package))
         (temporary-package (make-package "TEMPORARY")))
    (unwind-protect
         (progn
           (handler-bind ((package-error #'continue))
             (setf (fdefinition 'find-package) (lambda (designator)
                                                 (cond ((equal "SB!XC" designator)
                                                        temporary-package)
                                                       ((stringp designator)
                                                        (funcall original-find-package (substitute #\- #\! designator)))
                                                       (t (funcall original-find-package designator))))))
           (bind ((definition-source (sb-introspect:find-definition-source definition))
                  (pathname (sb-introspect:definition-source-pathname definition-source))
                  (source-path (sb-introspect:definition-source-form-path definition-source))
                  (first-index (car source-path)))
             (with-input-from-file (stream pathname :element-type 'character :external-format (sb-impl::default-external-format))
               (bind ((*readtable* (swank-backend::shebang-readtable))
                      (*package* (find-package :common-lisp-user)))
                 (iter (for index :from 0)
                       (for form = (handler-bind ((sb-int::simple-reader-package-error #'continue))
                                     (read stream #f stream)))
                       (for position = (file-position stream))
                       (for previous-position :previous position)
                       (until (eq form stream))
                       (when (member (first form) '(common-lisp:in-package hu.dwim.common:in-package))
                         (setf *package* (find-package (second form)))
                         (awhen (cdr (assoc (package-name *package*) swank:*readtable-alist* :test 'string=))
                           (setf *readtable* it)))
                       (when (= first-index index)
                         (file-position stream previous-position)
                         (return (string-trim-whitespace (iter (for char = (read-char stream nil stream))
                                                               (until (or (= position (file-position stream))
                                                                          (eq form stream)))
                                                               (collect char :result-type '(vector character)))))))))))
      (handler-bind ((package-error #'continue))
        (setf (fdefinition 'find-package) original-find-package)
        (delete-package temporary-package)))))

(def (function e) definition-source-text (definition)
  (or (gethash definition *definition-source-texts*)
      (setf (gethash definition *definition-source-texts*)
            (read-definition-source-text definition))))
