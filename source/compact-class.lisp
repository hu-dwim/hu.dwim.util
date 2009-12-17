;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;; TODO: use fixnums to store compact slot values not to confuse the gc

;;;;;;
;;; compact-class

(def (class e) compact-class (standard-class)
  ((compact-words-count
    :initform nil
    :type (or null integer)
    :accessor compact-words-count-of
    :documentation "Number of compact words used to store slot values.")
   (compact-words-offset
    :initform nil
    :type (or null integer)
    :accessor compact-words-offset-of
    :documentation "Offset of compact words in the slot vector.")))

(def class compact-slot-definition (standard-slot-definition)
  ())

(def class compact-direct-slot-definition (compact-slot-definition standard-direct-slot-definition)
  ())

(def class compact-effective-slot-definition (compact-slot-definition standard-effective-slot-definition)
  ((compact-word-offset
    :initform nil
    :type integer
    :accessor compact-word-offset-of
    :documentation "Offset of compact word in the slot vector.")
   (compact-bit-position
    :initform nil
    :type integer
    :accessor compact-bit-position-of
    :documentation "Bit position used within the compact word.")
   (compact-bit-size
    :initform nil
    :type integer
    :accessor compact-bit-size-of
    :documentation "Bit size used within the compact word.")))

(def method validate-superclass ((subclass compact-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

(def method direct-slot-definition-class ((class compact-class) &key allocation &allow-other-keys)
  (if (eq allocation :compact)
      (find-class 'compact-direct-slot-definition)
      (call-next-method)))

(def method effective-slot-definition-class ((class compact-class) &key allocation &allow-other-keys)
  (if (eq allocation :compact)
      (find-class 'compact-effective-slot-definition)
      (call-next-method)))

(def method finalize-inheritance :after ((class compact-class))
  (iter (with compact-words-count = 0)
        (with compact-words-offset = (setf (compact-words-offset-of class)
                                           (sb-pcl::wrapper-no-of-instance-slots (sb-pcl::class-wrapper class))))
        (with compact-slots = (remove-if-not (of-type 'compact-effective-slot-definition) (class-slots class)))
        (for compact-slot :in compact-slots)
        (for compact-type = (slot-definition-type compact-slot))
        (for compact-bit-size = (aif (type-instance-count-upper-bound compact-type)
                                     (ceiling (log it 2))
                                     (error "Unknown compact type ~A" compact-type)))
        (when (> compact-bit-size sb-vm::n-word-bits)
          (error "Compact bit size is too large for ~A" compact-slot))
        (setf (compact-bit-size-of compact-slot) compact-bit-size)
        (iter (for compact-word-offset :from compact-words-offset :to (+ 1 compact-words-offset compact-words-count))
              (for compact-bit-position = (or (iter (for compact-slot :in compact-slots)
                                                    (when (equal compact-word-offset (compact-word-offset-of compact-slot))
                                                      (maximize (+ (compact-bit-position-of compact-slot)
                                                                   (compact-bit-size-of compact-slot)))))
                                              0))
              (when (<= (+ compact-bit-position compact-bit-size) sb-vm::n-word-bits)
                (finish))
              (finally
               (setf (compact-word-offset-of compact-slot) compact-word-offset
                     (compact-bit-position-of compact-slot) compact-bit-position)
               (when (= compact-word-offset (+ compact-words-offset compact-words-count))
                 (incf compact-words-count))))
        (finally
         (setf (compact-words-count-of class) compact-words-count))))

(def method allocate-instance ((class compact-class) &rest args)
  (declare (ignore args))
  ;; NOTE: this is mostly copied over from SBCL
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (bind ((compact-word-count (compact-words-count-of class))
         (compact-word-offset (compact-words-offset-of class))
         (wrapper (sb-pcl::class-wrapper class))
         (instance (sb-pcl::%make-standard-instance nil (sb-pcl::get-instance-hash-code)))
         (slot-count (+ compact-word-count (sb-pcl::wrapper-no-of-instance-slots wrapper)))
         (slot-vector (make-array slot-count :initial-element sb-pcl::+slot-unbound+)))
    (iter (repeat compact-word-count)
          (for index :from compact-word-offset)
          (setf (aref slot-vector index) 0))
    (setf (sb-pcl::std-instance-wrapper instance) wrapper
          (sb-pcl::std-instance-slots instance) slot-vector)
    instance))

(def method slot-boundp-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  #t)

;; TODO: safety
(def method slot-value-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (bind ((compact-word-offset (compact-word-offset-of slot))
         (compact-bit-size (compact-bit-size-of slot))
         (compact-bit-position (compact-bit-position-of slot))
         (value (ldb (byte compact-bit-size compact-bit-position)
                     (standard-instance-access instance compact-word-offset)))
         (type (slot-definition-type slot)))
    (cond ((subtypep type 'boolean)
           (= value 1))
          ((subtypep type 'integer)
           value)
          ((subtypep type 'base-char)
           (code-char value))
          ((subtypep type 'single-float)
           (sb-vm::make-single-float value))
          ((and (subtypep type 'simple-base-string)
                (consp type))
           ;; TODO: should we include header and return a displaced string instead? would that hurt gc?
           (iter (with string = (make-string (second type)))
                 (for index :from 0 :below (second type))
                 (for position :initially 0 :then (+ 7 position))
                 (setf (aref string index) (code-char (ldb (byte 7 position) value)))
                 (finally (return string))))
          (t
           (aif (type-instance-count-upper-bound type)
                (elt (type-instance-list type) value)
                (error "Unknown compact type ~A" type))))))

;; TODO: safety
(def method (setf slot-value-using-class) (new-value (class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (bind ((compact-word-offset (compact-word-offset-of slot))
         (compact-bit-size (compact-bit-size-of slot))
         (compact-bit-position (compact-bit-position-of slot))
         (type (slot-definition-type slot)))
    (setf (standard-instance-access instance compact-word-offset)
          (dpb (cond ((subtypep type 'boolean)
                      (if new-value 1 0))
                     ((subtypep type 'integer)
                      new-value)
                     ((subtypep type 'base-char)
                      (char-code new-value))
                     ((subtypep type 'single-float)
                      (sb-vm::single-float-bits new-value))
                     ((and (subtypep type 'simple-base-string)
                           (consp type))
                      (iter (with value = 0)
                            (for char :in-sequence (coerce new-value 'simple-base-string))
                            (for position :initially 0 :then (+ 7 position))
                            (setf value (dpb (char-code char) (byte 7 position) value))
                            (finally (return value))))
                     (t
                      (if (type-instance-count-upper-bound type)
                          (position new-value (type-instance-list type))
                          (error "Unknown compact type ~A" type))))
               (byte compact-bit-size compact-bit-position)
               (standard-instance-access instance compact-word-offset)))))
