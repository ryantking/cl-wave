(in-package #:cl-wave)
;;;; cl-wave.lisp
;;;;
;;;; Core file for opening wave audio files

;;; Mapping of Chunk name to fields with name and size
(defparameter *chunks*
  (list (cons "RIFF" (list (cons :file-type nil)))
        (cons "fmt " (list (cons :compression-code 2)
                           (cons :num-channels 2)
                           (cons :sample-rate 4)
                           (cons :byte-rate 4)
                           (cons :sample-bytes 2)
                           (cons :sample-bits 2)
                           (cons :extension-size 2)))
        (cons "data" (list (cons :frames nil)))))

(defun default-params ()
  "Create a default set of parameters for writing waves."
  (list :file-type "WAVE"
        :compression-code 1
        :num-channels 1
        :sample-rate 44100
        :byte-rate 88200
        :sample-bytes 2
        :sample-bits 16
        :extension-size 0
        :frames '()
        :chunk-sizes (list (cons "RIFF" 36)
                           (cons "fmt " 16)
                           (cons "data" 0))))

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
  (setf (params wave) (read-chunks (io wave) *chunks*)))

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

(defmethod set-num-channels ((wave write-wave) num-channels)
  (setf (getf (params wave) :num-channels) num-channels))

(defmethod set-sample-rate ((wave write-wave) sample-rate)
  (setf (getf (params wave) :sample-rate) sample-rate))

(defmethod set-sample-width ((wave write-wave) bytes)
  (setf (getf (params wave) :sample-bytes) bytes)
  (setf (getf (params wave) :sample-bits) (* bytes 8)))

(defmethod set-frames ((wave write-wave) frames)
  (setf (getf (params wave) :frames) frames))

;;; Functions to open/close wave objects.
(defun open-wave (filename &key (direction :input))
  "Creates a new wave object with a pointer to its file stream."
  (cond ((eq direction :input)
         (make-instance 'read-wave
                        :io (open filename :element-type '(unsigned-byte 8))))
        ((eq direction :output)
         (make-instance 'write-wave
                        :io (open filename
                                  :element-type '(unsigned-byte 8)
                                  :direction :output
                                  :if-exists :supersede)
                        :params (default-params)))))

(defmethod close-wave ((wave read-wave) &key abort)
  "Closes a read-only wave file."
  (close (io wave) :abort abort))

(defmethod close-wave ((wave write-wave) &key abort)
  (when (getf (params wave) :extension-size)
    (incf (cdr (assoc "RIFF" (getf (params wave) :chunk-sizes) :test #'equal)) 2)
    (incf (cdr (assoc "fmt " (getf (params wave) :chunk-sizes) :test #'equal)) 2))
  (when (getf (params wave) :frames)
    (incf (cdr (assoc "RIFF" (getf (params wave) :chunk-sizes) :test #'equal))
          (* (length (getf (params wave) :frames))
             (getf (params wave) :sample-bytes)))
    (incf (cdr (assoc "data" (getf (params wave) :chunk-sizes) :test #'equal))
          (* (length (getf (params wave) :frames))
             (getf (params wave) :sample-bytes))))
  (write-chunks (io wave) *chunks* (params wave))
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
