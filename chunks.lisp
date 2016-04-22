(in-package #:cl-wave)
;;;; chunks.lisp
;;;;
;;;; Definitions of the chunks that compose a wave file.

;;; An associated-list with to store information about each chunk's fields
;;; Information is stored in the following format:
;;; (list (cons <chunk-id> (list (cons <field-name> <byte-size>))))
;;; No byte size is used to indicate a special field such as file-type or frames
(defparameter *chunks*
  (list (cons "RIFF" (list (cons :file-type nil)))
        (cons "fmt " (list (cons :compression-code 2)
                           (cons :num-channels 2)
                           (cons :sample-rate 4)
                           (cons :byte-rate 4)
                           (cons :sample-bytes 2)
                           (cons :sample-bits 2)
                           (cons :extension-size 2)))
        (cons "data" (list (cons :frames nil)))))

(defun default-params ()
  "Create a default set of parameters for writing waves."
  (list :file-type "WAVE"
        :compression-code 1
        :num-channels 1
        :sample-rate 44100
        :byte-rate 88200
        :sample-bytes 2
        :sample-bits 16
        :extension-size nil
        :frames '()
        :chunk-sizes (list (cons "RIFF" 36)
                           (cons "fmt " 16)
                           (cons "data" 0))))

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

(defun read-chunks (stream)
  "Creates a list of every property contained in all the chunks."
  (loop for chunk-id = (read-tag stream)
     while (and chunk-id fields)
     for fields = (cdr (assoc chunk-id *chunks* :test #'equal))
     append (read-chunk stream fields params) into params
     finally (return params)))

;;; Functions to handle writing chunks
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

(defun write-chunks (stream params)
  "Writes every chunk to a single wave file."
  (loop for chunk in *chunks*
     for chunk-size = (cdr (assoc (car chunk) (getf params :chunk-sizes) :test #'equal)) do
       (write-tag stream (car chunk))
       (write-uint stream chunk-size 4)
       (write-chunk stream (cdr chunk) params chunk-size)))
