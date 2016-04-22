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
  ((filename :initform ""
             :initarg :filename
             :accessor filename)
   (io :initform nil
           :initarg :io
           :accessor io)
   (num-channels :initform +num-channels+
                 :initarg :num-channels
                 :accessor num-channels)
   (sample-rate :initform +sample-rate+
                :initarg :sample-rate
                :accessor sample-rate)
   (bytes-per-second :initform +bytes-per-second+
                     :initarg :bytes-per-second
                     :accessor bytes-per-second)
   (bytes-per-sample :initform +bytes-per-sample+
                     :initarg :bytes-per-sample
                     :accessor bytes-per-sample)
   (bits-per-sample :initform +bits-per-sample+
                    :initarg :bits-per-sample
                    :accessor bits-per-sample)
   (frames :initform '()
           :accessor frames
           :initarg :frames)))

(defclass read-wave (wave) ())

(defclass write-wave (wave) ())

(defun open-wave (filename &key (direction :input))
  (cond ((eq direction :input)
          (let* ((in (open filename :element-type '(unsigned-byte 8)))
                 (riff-id (read-tag in))
                 (file-size (read-uint in 4))
                 (file-type (read-tag in))
                 (fmt-id (read-tag in))
                 (fmt-size (read-uint in 4))
                 (compression-code (read-uint in 2))
                 (num-channels (read-uint in 2))
                 (sample-rate (read-uint in 4))
                 (bytes-per-second (read-uint in 4))
                 (bytes-per-sample (read-uint in 2))
                 (bits-per-sample (read-uint in 4))
                 (data-id (read-tag in))
                 (data-size (read-uint in 4))
                 (value-range (expt 2 bits-per-sample))
                 (max-value (1- (/ value-range 2)))
                 (frames (loop repeat (/ data-size bytes-per-sample)
                            for frame = (read-uint in bytes-per-sample)
                            collect (if (> frame max-value)
                                        (- frame value-range)
                                        frame))))
            (if (and (equal riff-id "RIFF")
                     (equal fmt-id "fmt ")
                     (equal data-id "data")
                     (equal file-type "WAVE")
                     (= fmt-size 18)
                     (= compression-code 1))
                (make-instance 'read-wave
                               :filename filename
                               :io in
                               :num-channels num-channels
                               :sample-rate sample-rate
                               :bytes-per-second bytes-per-second
                               :bytes-per-sample bytes-per-sample
                               :bits-per-sample bits-per-sample
                               :frames frames)
                (error "Invalid Wave file."))))))

(defmethod close-wave ((wave-object wave) &key abort)
  (close (io wave-object) :abort abort))

(defmacro with-open-wave ((wave-object filename &key (direction :input)) &body body)
  (let ((g (gensym)))
    `(let ((,wave-object (open-wave ,filename
                               :direction ,direction))
           (,g t))
       (unwind-protect (progn ,@body (setf ,g nil))
         (when (io ,wave-object)
           (close-wave ,wave-object :abort ,g))))))

(defmethod get-sample-rate ((wave-object wave))
  (sample-rate wave-object))

(defmethod get-frame-length ((wave-object wave))
  (length (frames wave-object)))

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
