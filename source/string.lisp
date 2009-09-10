;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Whitespaces

(def (constant e :test #'equal) +whitespace-characters+ (list #\Space #\Tab #\NewLine))

(def (function e) string-trim-whitespace (text)
  (string-trim +whitespace-characters+ text))

;;;;;;
;;; Concatenate

;; TODO: rename this to string+
(def (function eo) concatenate-string (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro concatenate-string (&rest args)
  `(concatenate 'string ,@args))

;;;;;;
;;; Roman numeral

(def (function e) parse-roman-numeral-digit (char)
  (case char
    (#\I 1)
    (#\V 5)
    (#\X 10)
    (#\L 50)
    (#\C 100)
    (#\D 500)
    (#\M 1000)
    (t nil)))

(def (function e) roman-numeral-digit-p (char)
  (member char '(#\I #\V #\X #\L #\C #\D #\M)))

(def (function e) parse-roman-numeral (str &key (start 0) end)
  (iter (for index from start below (or end (length str)))
        (for ch = (elt str index))
        (for digit = (parse-roman-numeral-digit ch))
        (while digit)
        (for prev-digit previous digit)
        (for result first 0 then (if (>= prev-digit digit)
                                         (+ result prev-digit)
                                         (- result prev-digit)))
        (finally (return-from parse-roman-numeral (values (+ result (or digit prev-digit 0)) index)))))

(def (function e) string-with-numeric< (str1 str2 digit-p parse-number &key (start1 0) (start2 0))
  (bind ((num-start1 (position-if digit-p str1 :start start1))
         (num-start2 (position-if digit-p str2 :start start2)))
    (if (and (numberp num-start1)
             (numberp num-start2)
             (= num-start1 num-start2))
        (cond
          ((string< str1 str2 :start1 start1 :end1 num-start1 :start2 start2 :end2 num-start2) #t)
          ((string> str1 str2 :start1 start1 :end1 num-start1 :start2 start2 :end2 num-start2) #f)
          (t (bind ((num-end1 (position-if-not digit-p str1 :start num-start1))
                    (num-end2 (position-if-not digit-p str2 :start num-start2))
                    (num1 (funcall parse-number str1 :start num-start1 :end num-end1))
                    (num2 (funcall parse-number str2 :start num-start2 :end num-end2)))
               (cond
                 ((< num1 num2) #t)
                 ((> num1 num2) #f)
                 (t (string-with-numeric< str1 str2 :start1 num-end1 :start2 num-end2))))))

        (string< str1 str2 :start1 start1 :start2 start2))))

(def (function e) string-with-integers< (str1 str2 &key (start1 0) (start2 0))
  (string-with-numeric< str1 str2 #'digit-char-p #'parse-integer :start1 start1 :start2 start2))

(def (function e) string-with-roman-numerals< (str1 str2 &key (start1 0) (start2 0))
  (string-with-numeric< str1 str2 #'roman-numeral-digit-p #'parse-roman-numeral :start1 start1 :start2 start2))
