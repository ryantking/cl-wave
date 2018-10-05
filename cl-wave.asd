;;;; cl-wave.asd
;;;;
;;;; ASDF Configuration file

(asdf:defsystem #:cl-wave
  :description "A library for interfacing with wave audio files"
  :version "0.1.2"
  :author "Ryan King <rtking@bu.edu>"
  :license "MIT License"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "rtk-cl-utils")
               (:file "riff")
               (:file "wave")
               (:file "cl-wave")))
