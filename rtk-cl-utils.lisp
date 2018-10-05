;;;; utils
;;;;
;;;; See also:
;;;; https://github.com/RyanTKing/cl-wave/issues/1
;;;;
(in-package #:cl-wave)

(defmacro let1 ((var val) &body body)
  `(let ((,var ,val))
     ,@body))

(defparameter sint 0)
