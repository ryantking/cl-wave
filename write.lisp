;;;; write.lisp
;;;;
;;;; Functions for writing wave files

(in-package #:cl-wave)

(defmethod set-num-channels ((wave-object wave) n)
  (setf (num-channels wave-object) n))

(defmethod set-sample-width ((wave-object wave) n &key bits)
  (cond (bits
         (setf (bits-per-sample wave-object) n)
         (setf (bytes-per-sample wave-object) (/ n 8)))
        (t
         (setf (bytes-per-sample wave-object) n)
         (setf (bits-per-sample wave-object) (* n 8)))))

(defmethod set-sample-rate ((wave-object wave))
  (setf (sample-rate wave-object) n))

(defmethod set-frames ((wave-object wave) new-frames &key append)
  (if append
      (setf (frames wave-object) (append (frames wave-object) new-frames))
      (setf (frames wave-object) new-frames)))

(defun write-uint (stream uint n)
  (loop for i below n do
       (write-byte (ldb (byte 8 (* i 8)) uint) stream)))

(defun write-tag (stream tag)
  (loop for c across tag
     for i = 0 then (1+ i) do
       (write-byte (char-code c) stream)))

(defmethod write-file ((wave-object wave))
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
    (write-uint (io wave-object) (bits-per-sample wave-object) 4)
    (write-tag (io wave-object) +data-id+)
    (write-uint (io wave-object) (* data-size byte-width) 4)
    (loop for frame in (frames wave-object) do
         (write-uint (io wave-object) frame byte-width))))
