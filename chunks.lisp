(in-package #:cl-wave)
;;;; chunks.lisp
;;;;
;;;; Definitions of the chunks that compose a wave file.

;;; An alist to hold all the information about the fields associated with a particular chunk
;;; Chunks are associated using their ASCII four-character tags
(defparameter *chunks*
  (list (cons "RIFF" (list (list :name :file-type
                                 :reader #'read-tag
                                 :size 4
                                 :default "WAVE")))
        (cons "fmt " (list (list :name :compression-code
                                 :reader #'read-u2
                                 :size 2
                                 :default 1)
                           (list :name :num-channels
                                 :reader #'read-u2
                                 :size 2
                                 :default 1)
                           (list :name :sample-rate
                                 :reader #'read-u4
                                 :size 4
                                 :default 44100)
                           (list :name :byte-rate
                                 :reader #'read-u4
                                 :size 4
                                 :default 88200)
                           (list :name :sample-bytes
                                 :reader #'read-u2
                                 :size 2
                                 :default 2)
                           (list :name :sample-bits
                                 :reader #'read-u2
                                 :size 2
                                 :default 16)
                           (list :name :extension-size
                                 :reader #'read-u2
                                 :size 2
                                 :default 0)))
        (cons "data" (list (list :name :frames
                                 :reader #'read-frames
                                 :size 0
                                 :default '())))))

;;; Functions for Reading Chunks
(defun read-field (stream name reader params chunk-size)
  "Reads a single field from a chunk."
  (if (eq name :frames)
      (list name (funcall reader stream
                          (getf params :sample-bytes) chunk-size
                          (if (= (getf params :sample-bytes) 2)
                              #'read-s2
                              #'read-s4)))
    (list name (funcall reader stream))))

(defun read-chunk (stream fields params)
  "Creates a list of all of the properties contained in one chunk."
  (loop with chunk-size = (read-u4 stream)
     for field in fields
     for pos = (+ (or pos 0) (getf field :size))
     while (<= pos chunk-size)
     append (read-field stream (getf field :name) (getf field :reader) params chunk-size)))

(defun read-chunks (stream)
  "Creates a list of every property contained in all the chunks."
  (loop for chunk-id = (read-tag stream)
     for fields = (cdr (assoc chunk-id *chunks* :test #'equal))
     append (read-chunk stream fields params) into params
     while (and chunk-id fields)
     finally (return params)))
