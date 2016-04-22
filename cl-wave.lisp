;;;; cl-wave.lisp
;;;;
;;;; Core file for opening wave audio files

(in-package #:cl-wave)

(defun read-uint (stream n)
  "Reads an n-byte unsigned little-endian integer from stream."
  (loop for i below n
     with uint = 0 do
       (setf (ldb (byte 8 (* i 8)) uint) (read-byte stream nil))
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

