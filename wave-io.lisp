(in-package :cl-wave)
;;;; wave-io.lisp
;;;;
;;;; Functions for reading/writing to wave files stored on a disk.

;;; Reading Functions
(defun read-uint (stream bytes)
  (loop repeat bytes
     for n = 0 then (+ n 8)
     with uint = 0 do
       (setf (ldb (byte 8 n) uint) (read-byte stream))
       finally (return uint)))

(defun read-tag (stream)
  "Reads a 4-character ASCII tag from the stream."
  (loop repeat 4
     for byte = (read-byte stream nil)
     while byte
     collecting (code-char byte) into chars
     finally (when chars (return (coerce chars 'string)))))

(defun read-frames (stream size length)
  "Reads all the frames of a wave file."
  (loop for i = 0 then (+ i size)
     for frame = (read-uint stream size)
     with max-value = (1- (expt 2 (1- (* size 8))))
     with diff = (expt 2 (* size 8))
     while (< i length)
     collect (if (> frame max-value) (- frame diff) frame)))

;;; Writing Functions
(defun write-uint (stream uint bytes)
  (loop repeat bytes
     for n = 0 then (+ n 8) do
       (write-byte (ldb (byte 8 n) uint) stream))
  stream)

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do
       (write-byte (char-code ch) stream)) stream)

(defun write-frames (stream frames sample-width)
  (loop for frame in frames
     do (write-uint stream frame sample-width)))
