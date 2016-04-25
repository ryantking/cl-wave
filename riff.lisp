(in-package #:cl-wave)
;;;; chunks.lisp
;;;;
;;;; Functions for reading/writing to RIFF files

;;; Functions to handle reading chunks
(defun read-field (stream field params chunk-size)
  "Reads a single field from a chunk."
  (cond ((integerp (cdr field))
         (list (cdr field) (car field) (read-uint stream (cdr field))))
        ((eq (car field) :frames)
         (list chunk-size
               (car field)
               (read-frames stream (getf params :sample-bytes) chunk-size)))
        (t
         (list 4 (car field) (read-tag stream)))))

(defun read-chunk (stream fields params)
  "Creates a list of all of the properties contained in one chunk."
  (loop with chunk-size = (read-uint stream 4)
     for field in fields
     for value = (read-field stream field params chunk-size)
     for pos = (+ (or pos 0) (pop value))
     while (<= pos chunk-size)
     append value))

(defun read-chunks (stream chunk-map)
  "Creates a list of every property contained in all the chunks."
  (loop for chunk-id = (read-tag stream)
     while (assoc chunk-id chunk-map :test #'equal)
     for chunk-size = (read-uint stream 4)
     for fields = (cdr (assoc chunk-id chunk-map :test #'equal))
     for pos = 0
     append (loop while (< pos chunk-size)
               for field in fields
               for value = (read-field stream field params chunk-size)
               append (progn (incf pos (pop value)) value)) into params
     finally (return params)))

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
