(in-package #:cl-wave)
;;;; chunks.lisp
;;;;
;;;; Functions for reading/writing to RIFF files

;;; Functions for Reading/Writing Binary
(defun read-uint (stream bytes &optional (eof-error-p t))
  (loop repeat bytes
     for n = 0 then (+ n 8)
     with uint = 0 do
       (setf (ldb (byte 8 n) uint) (read-byte stream eof-error-p))
     finally (return uint)))

(defun read-tag (stream)
  "Reads a 4-character ASCII tag from the stream."
  (loop repeat 4
     for byte = (read-byte stream nil)
     while byte
     collecting (code-char byte) into chars
     finally (when chars (return (coerce chars 'string)))))

(defun write-uint (stream uint bytes)
  (loop repeat bytes
     for n = 0 then (+ n 8) do
       (write-byte (ldb (byte 8 n) uint) stream))
  stream)

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do
       (write-byte (char-code ch) stream)) stream)

;;; Functions to handle reading chunks
(defun default-parser (stream chunk-id chunk-size chunks)
  (loop repeat chunk-size
                 collect (read-uint stream 1)))

(defun read-chunk (stream parser chunks)
  (when-let* ((chunk-id (read-tag stream))
              (chunk-size (read-uint stream 4))
              (chunk-data (if (string= chunk-id "RIFF")
                              (read-tag stream)
                              (funcall parser stream chunk-id chunk-size chunks))))
    (list :chunk-id chunk-id
          :chunk-size chunk-size
          :chunk-data chunk-data)))

(defun read-chunks (stream &optional (parser #'default-parser))
  "Reads every chunk in a given RIFF file using the parsers provided"
  (loop for chunk = (read-chunk stream parser chunks)
       unless chunk return chunks
       collect chunk into chunks))

(defun write-field (stream field params)
  "Writes a single field to a wave file."
  (cond ((integerp (cdr field))
         (write-uint stream (getf params (car field)) (cdr field)) (cdr field))
        ((eq (car field) :frames)
         (write-frames stream (getf params (car field)) (getf params :sample-bytes))
         (* (length (getf params :frames)) (getf params :sample-bytes)))
        (t
         (write-tag stream (getf params (car field))) 4)))

(defun write-chunk (stream fields params chunk-size)
  "Writes a chunk to a wave file."
  (loop for field in fields
     for size = (write-field stream field params)
     for pos = (+ (or pos 0) size)
     while (< pos chunk-size)))

(defun write-chunks (stream chunk-map params)
  "Writes every chunk to a single wave file."
  (loop for chunk in chunk-map
     for chunk-size = (cdr (assoc (car chunk) (getf params :chunk-sizes) :test #'equal)) do
       (write-tag stream (car chunk))
       (write-uint stream chunk-size 4)
       (write-chunk stream (cdr chunk) params chunk-size)))
