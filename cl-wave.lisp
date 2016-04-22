;;;; cl-wave.lisp
;;;;
;;;; Core file for opening wave audio files

(in-package #:cl-wave)

(defconstant +num-channels+ 1)
(defconstant +sample-rate+ 44100)
(defconstant +bytes-per-second+ 88200)
(defconstant +bytes-per-sample+ 2)
(defconstant +bits-per-sample+ 16)

(defun read-uint (stream n)
  "Reads an n-byte unsigned little-endian integer from stream."
  (loop for i below n
     with uint = 0 do
       (setf (ldb (byte 8 (* i 8)) uint) (read-byte stream))
     finally (return uint)))

(defun read-tag (stream)
  "Reads a four character tag from the stream and returns a string
   or nil if its the end of the file."
  (loop repeat 4
     for byte = (read-byte stream nil)
     while byte
     collecting (code-char byte) into chars
     finally
       (when chars (return (coerce chars 'string)))))

(defclass wave ()
  ((fname :initform "" :accessor fname)
   (stream :initform nil :accessor stream)
   (num-channels :initform +num-channels+ :accessor num-channels)
   (sample-rate :initform +sample-rate+ :accessor sample-rate)
   (bytes-per-second :initform +bytes-per-second+ :accessor bytes-per-second)
   (bytes-per-sample :initform +bytes-per-sample+ :accessor bytes-per-sample)
   (bits-per-sample :initform +bits-per-sample+ :accessor bits-per-sample)
   (frames :initform '() :accessor frames)))

(defclass read-wave (wave) ())

(defclass write-wave (wave) ())

(defun make-wave ()
  (list :file-size 0
        :file-type ""
        :fmt-size 0
        :compression-code 0
        :number-of-channels 0
        :sample-rate 0
        :bytes-per-second 0
        :bytes-per-sample 0
        :bits-per-sample 0
        :data-size 0
        :frames '()))

(defun read-wave (stream)
  (loop for chunk-id = (read-tag stream)
     with wave = (make-wave)
       with others = 0
     while chunk-id do
       (cond ((equal chunk-id "RIFF")
              (setf (getf wave :file-size) (read-uint stream 4))
              (setf (getf wave :file-type) (read-tag stream)))
             ((equal chunk-id "fmt ")
              (setf (getf wave :fmt-size) (read-uint stream 4))
              (setf (getf wave :compression-code) (read-uint stream 2))
              (setf (getf wave :number-of-channels) (read-uint stream 2))
              (setf (getf wave :sample-rate) (read-uint stream 4))
              (setf (getf wave :bytes-per-second) (read-uint stream 4))
              (setf (getf wave :bytes-per-sample) (read-uint stream 2))
              (setf (getf wave :bits-per-sample) (read-uint stream 4)))
             ((equal chunk-id "data")
              (let* ((bps (getf wave :bytes-per-sample))
                     (diff (expt 2 (getf wave :bits-per-sample)))
                     (max (1- (/ diff 2)))
                     (data-size (read-uint stream 4)))
                (setf (getf wave :data-size) data-size)
                (setf (getf wave :frames)
                      (loop for frame = (read-uint stream bps)
                         repeat (/ data-size bps)
                         collect (if (> frame max) (- frame diff) frame))))))
       finally (return wave)))
