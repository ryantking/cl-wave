;;;; package.lisp
;;;;
;;;; Basic package configuration file.

(defpackage #:cl-wave
  (:use #:cl #:rtk #:alexandria)
  (:export #:open-wave
           #:close-wave
           #:with-open-wave
           #:get-num-channels
           #:get-sample-rate
           #:get-sample-width
           #:get-num-frames
           #:get-params
           #:get-frames
           #:set-num-channels
           #:set-sample-rate
           #:set-sample-width
           #:set-frames))

