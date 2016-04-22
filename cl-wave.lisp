;;;; cl-wave.lisp
;;;;
;;;; Core file for opening wave audio files

(in-package #:cl-wave)

(defconstant +riff-id+ "RIFF")
(defconstant +file-size+ 38)
(defconstant +file-type+ "WAVE")

(defconstant +fmt-id+ "fmt ")
(defconstant +fmt-size+ 18)
(defconstant +compression-code+ 1)
(defconstant +num-channels+ 1)
(defconstant +sample-rate+ 44100)
(defconstant +bytes-per-second+ 88200)
(defconstant +bytes-per-sample+ 2)
(defconstant +bits-per-sample+ 16)
(defconstant +extension-size+ 0)

(defconstant +data-id+ "data")
(defconstant +data-size+ 0)

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

(defun write-uint (stream uint n)
  (loop for i below n do
       (write-byte (ldb (byte 8 (* i 8)) uint) stream)))

(defun write-tag (stream tag)
  (loop for c across tag
     for i = 0 then (1+ i) do
       (write-byte (char-code c) stream)))

(defclass wave ()
  ((filename :initform ""
             :initarg :filename
             :accessor filename)
   (io :initform nil
           :initarg :io
           :accessor io)
   (fmt-size :initform +fmt-size+
             :initarg :fmt-size
             :accessor fmt-size)
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
   (extension-size :initform +extension-size+
                   :initarg :extension-size
                   :accessor extension-size)
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
                 (bits-per-sample (read-uint in 2))
                 (extension-size (if (= fmt-size 18)
                                     (read-uint in 2)
                                     0))
                 (data-id (read-tag in))
                 (data-size (read-uint in 4))
                 (value-range (expt 2 bits-per-sample))
                 (max-value (1- (/ value-range 2)))
                 (frames (loop repeat (/ data-size bytes-per-sample)
                            for frame = (read-uint in bytes-per-sample)
                            collect (if (> frame max-value)
                                        (- frame value-range)
                                        frame))))
            (if (and (equal riff-id +riff-id+)
                     (equal fmt-id +fmt-id+)
                     (equal data-id +data-id+)
                     (equal file-type +file-type+)
                     (= fmt-size +fmt-size+)
                     (= compression-code +compression-code+))
                (make-instance 'read-wave
                               :filename filename
                               :io in
                               :fmt-size fmt-size
                               :num-channels num-channels
                               :sample-rate sample-rate
                               :bytes-per-second bytes-per-second
                               :bytes-per-sample bytes-per-sample
                               :bits-per-sample bits-per-sample
                               :extension-size extension-size
                               :frames frames)
                (error "Invalid Wave file."))))
        ((eq direction :output)
         (make-instance 'read-wave
                        :filename filename
                        :io (open filename
                                  :element-type '(unsigned-byte 8)
                                  :direction :output
                                  :if-exists :supersede)))))

(defmethod close-wave ((wave-object read-wave) &key abort)
  (close (io wave-object) :abort abort))

(defmethod close-wave ((wave-object write-wave) &key abort)
  (let* ((byte-width (bytes-per-sample wave-object))
         (data-size (length (frames wave-object))))
    (write-tag (io wave-object) +riff-id+)
    (write-uint (io wave-object) (+ +file-size+ data-size) 4)
    (write-tag (io wave-object) +file-type+)
    (write-tag (io wave-object) +fmt-id+)
    (write-uint (io wave-object) +fmt-size+ 4)
    (write-uint (io wave-object) +compression-code+ 2)
    (write-uint (io wave-object) (num-channels wave-object) 2)
    (write-uint (io wave-object) (sample-rate wave-object) 4)
    (write-uint (io wave-object) (bytes-per-second wave-object) 4)
    (write-uint (io wave-object) byte-width 2)
    (write-uint (io wave-object) (bits-per-sample wave-object) 2)
    (when (= (fmt-size wave-object) 18)
      (write-uint (io wave-object) (extension-size wave-object) 2))
    (write-tag (io wave-object) +data-id+)
    (write-uint (io wave-object) (* data-size byte-width) 4)
    (loop for frame in (frames wave-object) do
         (write-uint (io wave-object) frame byte-width))
    (close (io wave-object) :abort abort)))

(defmacro with-open-wave ((wave-object filename &key (direction :input)) &body body)
  (let ((g (gensym)))
    `(let ((,wave-object (open-wave ,filename
                               :direction ,direction))
           (,g t))
       (unwind-protect (progn ,@body (setf ,g nil))
         (when (io ,wave-object)
           (close-wave ,wave-object :abort ,g))))))
