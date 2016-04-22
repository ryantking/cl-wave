(in-package :cl-wave)
;;;; wave-io.lisp
;;;;
;;;; Functions for reading/writing to wave files stored on a disk.

;;; Reading Functions
(defun read-u2 (stream)
  "Reads a 2-byte unsigned integer from the stream."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    (setf (ldb (byte 8 8) u2) (read-byte stream)) u2))

(defun read-u4 (stream)
  "Reads a 4-byte unsigned integer from the stream."
  (let ((u4 0))
    (setf (ldb (byte 8 0) u4) (read-byte stream))
    (setf (ldb (byte 8 8) u4) (read-byte stream))
    (setf (ldb (byte 8 16) u4) (read-byte stream))
    (setf (ldb (byte 8 24) u4) (read-byte stream)) u4))

(defun read-s2 (stream)
  "Reads a 2-byte signed integer from the stream."
  (let ((s2 (read-u2 stream)))
    (if (> s2 32767) (- s2 65536) s2)))

(defun read-s4 (stream)
  "Reads a 4-byte signed integer from the stream."
  (let ((s4 (read-u4 stream)))
    (if (> s4 2147483647) (- s4 4294967296) s4)))

(defun read-tag (stream)
  "Reads a 4-character ASCII tag from the stream."
  (loop repeat 4
     for byte = (read-byte stream nil)
     while byte
     collecting (code-char byte) into chars
     finally (when chars (return (coerce chars 'string)))))

(defun read-frames (stream size length reader-fn)
  "Reads all the frames of a wave file."
  (loop for i = 0 then (+ i size)
     while (< i length)
     collect (funcall reader-fn stream)))

;;; Writing Functions
(defun write-u2 (stream u2)
  "Writes a 2-byte unsigned integer to the stream."
  (write-byte (ldb (byte 8 0) u2) stream)
  (write-byte (ldb (byte 8 8) u2) stream) stream)

(defun write-u4 (stream u4)
  "Writes a 4-byte unsigned integer to the stream."
  (write-byte (ldb (byte 8 0) u4) stream)
  (write-byte (ldb (byte 8 8) u4) stream)
  (write-byte (ldb (byte 8 16) u4) stream)
  (write-byte (ldb (byte 8 24) u4) stream) stream)

(defun write-s2 (stream s2)
  "Writes a 2-byte signed integer to the stream."
  (when (< s2 0)
    (incf s2 65536))
  (write-u2 (stream s2)) stream)

(defun write-s4 (stream s4)
  "Writes a 4-byte signed integer to the stream."
  (when (< s4 0)
    (incf s4 4294967296))
  (write-u4 (stream s2)) stream)

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do
       (write-byte (char-code ch) stream)) stream)
