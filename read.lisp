;;;; read.lisp
;;;;
;;;; Functions for reading the various properties of wave files

(in-package #:cl-wave)

(defmethod get-num-channels ((wave-object wave))
  (num-channels wave-object))

(defmethod get-sample-rate ((wave-object wave))
  (sample-rate wave-object))

(defmethod get-sample-width ((wave-object wave) &key bits)
  (if bits
      (bits-per-sample wave-object)
      (bytes-per-sample wave-object)))

(defmethod get-num-frames ((wave-object wave))
  (length (frames wave-object)))

(defmethod get-params ((wave-object wave))
  (list :num-channels (num-channels wave-object)
        :sample-rate (sample-rate wave-object)
        :bytes-per-sample (bytes-per-sample wave-object)
        :bits-per-sample (bits-per-sample wave-object)
        :num-frames (length (frames wave-object))))

(defmethod get-frames ((wave-object wave) &key (start 0) (end 0 endp))
  (if endp
      (subseq (frames wave-object) start end)
      (subseq (frames wave-object) start)))

