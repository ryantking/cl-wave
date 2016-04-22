(in-package #:cl-wave)
;;;; chunks.lisp
;;;;
;;;; Definitions of the chunks that compose a wave file.

;;; An alist to hold all the information about the fields associated with a particular chunk
;;; Chunks are associated using their ASCII four-character tags
(defparameter *chunks*
  (list (cons "RIFF" (list (list :name :chunk-size
                                 :reader #'read-u4
                                 :size 4
                                 :default 36)
                           (list :name :file-type
                                 :reader #'read-tag
                                 :size 4
                                 :default "WAVE")))
        (cons "fmt " (list (list :name :chunk-size
                                 :reader #'read-u4
                                 :size 4
                                 :default 16)
                           (list :name :compression-code
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
        (cons "data" (list (list :name :chunk-size
                                 :reader #'read-u4
                                 :size 4
                                 :default 0)
                           (list :name :frames
                                 :reader #'read-frames
                                 :size 0
                                 :default '())))))
