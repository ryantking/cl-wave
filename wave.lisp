;;;; wave.lisp
;;;;
;;;; Wave class definition and methods
(in-package #:cl-wave)

;;; Wave Class
(defclass wave ()
  ((io :initarg :io :accessor io)
   (chunks :initform (list (list :chunk-id "RIFF"
                                 :chunk-size 36
                                 :chunk-data "WAVE")
                           (list :chunk-id "fmt "
                                 :chunk-size 16
                                 :chunk-data (list :compression-code 1
                                                   :num-channels 1
                                                   :sample-rate 44100
                                                   :byte-rate 88200
                                                   :sample-bytes 2
                                                   :sample-bits 16))
                           (list :chunk-id "data"
                                 :chunk-size 0
                                 :chunk-data '()))
           :accessor chunks)
   (params :accessor params)
   (frames :accessor frames))
  (:documentation "General wave class for holding pointer to io stream and params."))

(defclass read-wave (wave) ()
  (:documentation "Class for reading wave files."))

(defclass write-wave (wave) ()
  (:documentation "Class for writing wave files."))

;;; Wave Methods
(defmethod initialize-instance :after ((wave read-wave) &key)
  "Reads the wave's binary data the sets its parameters/frames accordingly."
  (setf (chunks wave) (read-chunks (io wave) #'wave-parser))
  (setf (params wave) (getf (cadr (chunks wave)) :chunk-data))
  (setf (frames wave) (getf (caddr (chunks wave)) :chunk-data)))

(defmethod initialize-instance :after ((wave write-wave) &key)
  "Creates a wave with the default parameters and no frames."
  (setf (params wave) (getf (cadr (chunks wave)) :chunk-data))
  (setf (frames wave) (getf (caddr (chunks wave)) :chunk-data)))

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
  (length (frames wave)))

(defmethod get-params ((wave wave))
  "Returns several parameters in a property-list."
  (list :num-channels (get-num-channels wave)
        :sample-rate (get-sample-rate wave)
        :sample-width (get-sample-width wave)
        :num-frames (get-num-frames wave)))

(defmethod get-frames ((wave wave) &key (start 0) (end nil))
  "Returns a list of all the frames or a subset of them."
  (if end
      (subseq (frames wave) start end)
      (subseq (frames wave) start)))

(defmethod set-num-channels ((wave write-wave) num-channels)
  "Sets the number of channels."
  (setf (getf (params wave) :num-channels) num-channels))

(defmethod set-sample-rate ((wave write-wave) sample-rate)
  "Sets the sample rate."
  (setf (getf (params wave) :sample-rate) sample-rate))

(defmethod set-sample-width ((wave write-wave) bytes)
  "Sets the sample width."
  (setf (getf (params wave) :sample-bytes) bytes)
  (setf (getf (params wave) :sample-bits) (* bytes 8)))

(defmethod set-frames ((wave write-wave) new-frames)
  "Sets the frames."
  (setf (frames wave) new-frames))

;;; Parser/Printer for Wave Binary
(defun wave-parser (stream chunk-id chunk-size chunks)
  "Parsing function for wave's RIFF chunks."
  (cond ((string= chunk-id "fmt ")
         (append (list :compression-code (read-uint stream 2)
                       :num-channels (read-uint stream 2)
                       :sample-rate (read-uint stream 4)
                       :byte-rate (read-uint stream 4)
                       :sample-bytes (read-uint stream 2)
                       :sample-bits (read-uint stream 2))
                 (when (= chunk-size 18)
                   (list :extension-size (read-uint stream 2)))))
        ((string= chunk-id "data")
         (loop with bytes = (getf (getf (cadr chunks) :chunk-data) :sample-bytes)
            repeat (/ chunk-size bytes)
            collect (read-sint stream bytes)))
        (t
         (default-parser stream chunk-id chunk-size chunks))))

(defun wave-printer (stream chunk-id chunk-size chunk-data chunks)
  "Printing function for wave's RIFF chunks."
  (cond ((string= chunk-id "fmt ")
         (write-uint stream (getf chunk-data :compression-code) 2)
         (write-uint stream (getf chunk-data :num-channels) 2)
         (write-uint stream (getf chunk-data :sample-rate) 4)
         (write-uint stream (getf chunk-data :byte-rate) 4)
         (write-uint stream (getf chunk-data :sample-bytes) 2)
         (write-uint stream (getf chunk-data :sample-bits) 2)
         (when (= chunk-size 18)
           (write-uint stream (getf chunk-data :extension-size) 2)))
        ((string= chunk-id "data")
         (loop with bytes = (getf (getf (cadr chunks) :chunk-data) :sample-bytes)
            for frame in chunk-data do (write-sint stream frame bytes)))
        (t
         (default-printer stream chunk-id chunk-size chunk-data chunks))))
