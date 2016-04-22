(in-package #:cl-wave)
;;;; cl-wave.lisp
;;;;
;;;; Core file for opening wave audio files

;;; Class Definitions
(defclass wave ()
  ((io :initarg :io :accessor io)
   (params :initarg :params :accessor params))
  (:documentation "General wave class for holding pointer to io stream and params."))

(defclass read-wave (wave) ()
  (:documentation "Class for reading wave files."))

(defclass write-wave (wave) ()
  (:documentation "Class for writing wave files."))

;;; Wave Methods
(defmethod initialize-instance :after ((wave read-wave) &key)
  "Sets the parameters of the wave by reading all its chunks on initialization."
  (setf (params wave) (read-chunks (io wave))))

(defmethod get-num-channels ((wave wave))
  "Returns the number of channels the wave object has."
  (getf (params wave) :num-channels))

(defmethod get-sample-rate ((wave wave))
  "Returns the sample rate of the wave object."
  (getf (params wave) :sample-rate))

(defmethod get-sample-width ((wave wave) &key bits)
  "Returns the sample width in bytes (or bits if specified)."
  (getf (params wave) (if bits :sample-bits :sample-bytes)))

(defmethod get-num-frames ((wave wave))
  "Returns the number of frames."
  (length (getf (params wave) :frames)))

(defmethod get-params ((wave wave))
  "Returns several parameters in a property-list."
  (list :num-channels (get-num-channels wave)
        :sample-rate (get-sample-rate wave)
        :sample-width (get-sample-width wave)
        :num-frames (get-num-frames wave)))

(defmethod get-frames ((wave wave) &key (start 0) (end nil))
  "Returns a list of all the frames or a subset of them."
  (if end
      (subseq (getf (params wave) :frames) start end)
      (subseq (getf (params wave) :frames) start)))

;;; Functions to open/close wave objects.
(defun open-wave (filename &key (direction :input))
  "Creates a new wave object with a pointer to its file stream."
  (cond ((eq direction :input)
         (make-instance 'read-wave
                        :io (open filename :element-type '(unsigned-byte 8))))))

(defmethod close-wave ((wave read-wave) &key abort)
  "Closes a read-only wave file."
  (close (io wave) :abort abort))

(defmacro with-open-wave ((wave-object filename &key (direction :input)) &body body)
  "Uses an unwind-protect to ensure the wave stream is closed in the event of an error."
  (let ((g (gensym)))
    `(let ((,wave-object (open-wave ,filename
                                    :direction ,direction))
           (,g t))
       (unwind-protect (progn ,@body (setf ,g nil))
         (when (io ,wave-object)
           (close-wave ,wave-object :abort ,g))))))
