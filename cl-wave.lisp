;;;; cl-wave.lisp
;;;;
;;;; Main functions to open and close waves
(in-package #:cl-wave)

(defun open-wave (filename &key (direction :input))
  "Creates a new wave object with a pointer to its file stream."
  (cond ((eq direction :input)
         (make-instance 'read-wave
                        :io (open filename
                                  :element-type '(unsigned-byte 8))))
        ((eq direction :output)
         (make-instance 'write-wave
                        :io (open filename
                                  :element-type '(unsigned-byte 8)
                                  :direction :output
                                  :if-exists :supersede)))))

(defmethod close-wave ((wave read-wave) &key abort)
  "Closes a read-only wave file."
  (close (io wave) :abort abort))

(defmethod close-wave ((wave write-wave) &key abort)
  "Writes a wave to a file then closes the stream."
  (let* ((data-size (* (get-num-frames wave)
                       (getf (params wave) :sample-bytes)))
         (fmt-size (if (getf (params wave) :extension-size) 18 16))
         (riff-size (+ 4 data-size fmt-size)))
    (setf (getf (car (chunks wave)) :chunk-size) riff-size)
    (setf (getf (cadr (chunks wave)) :chunk-size) fmt-size)
    (setf (getf (cadr (chunks wave)) :chunk-data) (params wave))
    (setf (getf (caddr (chunks wave)) :chunk-size) data-size)
    (setf (getf (caddr (chunks wave)) :chunk-data) (frames wave))
    (write-chunks (io wave) (chunks wave) #'wave-printer)
    (close (io wave) :abort abort)))

(defmacro with-open-wave ((wave-object filename &key (direction :input)) &body body)
  "Uses an unwind-protect to ensure the wave stream is closed in the event of an error."
  (let ((g (gensym)))
    `(let ((,wave-object (open-wave ,filename
                                    :direction ,direction))
           (,g t))
       (unwind-protect (progn ,@body (setf ,g nil))
         (when (io ,wave-object)
           (close-wave ,wave-object :abort ,g))))))
