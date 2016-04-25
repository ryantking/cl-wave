(in-package #:cl-wave)
;;;; cl-wave.lisp
;;;;
;;;; Main functions to open and close waves

(defun open-wave (filename &key (direction :input))
  "Creates a new wave object with a pointer to its file stream."
  (cond ((eq direction :input)
         (make-instance 'read-wave
                        :io (open filename
                                  :element-type '(unsigned-byte 8))))))

(defmethod close-wave ((wave read-wave) &key abort)
  "Closes a read-only wave file."
  (close (io wave) :abort abort))

(defmethod close-wave ((wave write-wave) &key abort)
  (when (getf (params wave) :extension-size)
    (incf (cdr (assoc "RIFF" (getf (params wave) :chunk-sizes) :test #'equal)) 2)
    (incf (cdr (assoc "fmt " (getf (params wave) :chunk-sizes) :test #'equal)) 2))
  (when (getf (params wave) :frames)
    (incf (cdr (assoc "RIFF" (getf (params wave) :chunk-sizes) :test #'equal))
          (* (length (getf (params wave) :frames))
             (getf (params wave) :sample-bytes)))
    (incf (cdr (assoc "data" (getf (params wave) :chunk-sizes) :test #'equal))
          (* (length (getf (params wave) :frames))
             (getf (params wave) :sample-bytes))))
  (write-chunks (io wave) *chunks* (params wave))
  (close (io wave) :abort abort))

(defmacro with-open-wave ((wave-object filename &key (direction :input)) &body body)
  "Uses an unwind-protect to ensure the wave stream is closed in the event of an error."
  (let ((g (gensym)))
    `(let ((,wave-object (open-wave ,filename
                                    :direction ,direction))
           (,g t))
       (unwind-protect (progn ,@body (setf ,g nil))
         (when (io ,wave-object)
           (close-wave ,wave-object :abort ,g))))))
