;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; compact-class

(def (class e) compact-class (standard-class)
  ((compact-slot-location :type integer :accessor compact-slot-location-of)))

(def class compact-slot-definition (standard-slot-definition)
  ())

(def class compact-direct-slot-definition (compact-slot-definition standard-direct-slot-definition)
  ())

(def class compact-effective-slot-definition (compact-slot-definition standard-effective-slot-definition)
  ((bit-size :type integer :accessor bit-size-of)
   (bit-position :type integer :accessor bit-position-of)))

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
  (dolist (slot (class-slots class))
    (when (typep slot 'compact-effective-slot-definition)
      (setf (compact-slot-location-of class) (sb-pcl::wrapper-no-of-instance-slots (sb-pcl::class-wrapper class))
            (bit-size-of slot) (slot-allocation-bit-size slot)
            (bit-position-of slot) (slot-allocation-bit-position class slot)))))

(def method allocate-instance ((class compact-class) &rest args)
  (declare (ignore args))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (bind ((wrapper (sb-pcl::class-wrapper class))
         (instance (sb-pcl::%make-standard-instance nil (sb-pcl::get-instance-hash-code)))
         (slot-count (1+ (sb-pcl::wrapper-no-of-instance-slots wrapper))))
    (setf (sb-pcl::std-instance-wrapper instance) wrapper
          (sb-pcl::std-instance-slots instance) (prog1-bind slot-array (make-array slot-count :initial-element sb-pcl::+slot-unbound+)
                                                  (setf (aref slot-array (compact-slot-location-of class)) 0)))
    instance))

(def method slot-boundp-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  #t)

(def method slot-value-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (bind ((bit-size (bit-size-of slot))
         (bit-position (bit-position-of slot))
         (type (slot-definition-type slot))
         (location (compact-slot-location-of class))
         (value (ldb (byte bit-size bit-position) (standard-instance-access instance location))))
    (cond ((subtypep type 'boolean)
           (= value 1))
          ((subtypep type 'integer)
           value)
          ((subtypep type 'base-char)
           (code-char value))
          ((subtypep type 'single-float)
           (sb-vm::make-single-float value))
          (t
           (error "Unknown compact type ~A" type)))))

(def method (setf slot-value-using-class) (new-value (class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (bind ((bit-size (bit-size-of slot))
         (bit-position (bit-position-of slot))
         (type (slot-definition-type slot))
         (location (compact-slot-location-of class)))
    (setf (standard-instance-access instance location)
          (dpb (cond ((subtypep type 'boolean)
                      (if new-value 1 0))
                     ((subtypep type 'integer)
                      new-value)
                     ((subtypep type 'base-char)
                      (char-code new-value))
                     ((subtypep type 'single-float)
                      (sb-vm::single-float-bits new-value))
                     (t
                      (error "Unknown compact type ~A" type)))
               (byte bit-size bit-position) (standard-instance-access instance location)))))

;;;;;;
;;; Util

(def function type-allocation-bit-size (type)
  (cond ((subtypep type 'boolean)
         1)
        ((subtypep type '(integer 0 255))
         8)
        ((subtypep type 'base-char)
         7)
        ((subtypep type 'single-float)
         32)
        (t
         (error "Unknown compact type ~A" type))))

(def function slot-allocation-bit-size (slot)
  (type-allocation-bit-size (slot-definition-type slot)))

(def function slot-allocation-bit-position (class slot)
  (iter (for class-slot :in (class-slots class))
        (when (eq class-slot slot)
          (return position))
        (when (typep class-slot 'compact-effective-slot-definition)
          (bind ((bit-size (slot-allocation-bit-size class-slot)))
            ;; TODO: allow using more slots in a single instance spanning through multiple bytes/words in the slot vector
            (when (> (+ bit-size position) sb-vm::n-word-bits)
              (error "Cannot fit into a single word"))
            (summing bit-size :into position)))))
