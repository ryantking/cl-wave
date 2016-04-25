;;;; chunks.lisp
;;;;
;;;; Functions for reading/writing to RIFF files
(in-package #:cl-wave)

;;; Functions to read/write binary files
(defun read-uint (stream bytes)
  "Reads an unsigned integer from the stream with the specified number of bytes."
  (loop for n below (* bytes 8) by 8
     with uint = 0 do (setf (ldb (byte 8 n) uint) (read-byte stream))
     finally (return uint)))

(defun read-sint (stream bytes)
  "Reads a signed integer from the stream with the specified number of bytes."
  (let1 (sint (read-uint stream bytes))
    (if (> sint (1- (expt 2 (1- (* bytes 8)))))
        (- sint (expt 2 (* bytes 8)))
        sint)))

(defun read-tag (stream)
  "Reads a 4-character ASCII tag from the stream."
  (loop repeat 4
     for byte = (read-byte stream nil)
     if byte collect (code-char byte) into chars
     else return nil end
     finally (return (coerce chars 'string))))

(defun write-uint (stream uint bytes)
  "Writes an unsigned integer to the stream with the specified number of bytes."
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) uint) stream)))

(defun write-sint (stream sint bytes)
  "Writes a signed integer to the stream with the specified number of bytes."
  (when (< sint 0) (incf sint (expt 2 (* bytes 8))))
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) sint) stream)))

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do (write-byte (char-code ch) stream)))

;;; Functions to read/write RIFF chunks
(defun default-parser (stream chunk-id chunk-size chunks)
  "Parses a chunk by reading the data into a list of 1-byte unsigned integers."
  (loop repeat chunk-size collect (read-uint stream 1)))

(defun read-chunk (stream parser chunks)
  "Reads a single RIFF chunk using the specified parsing function."
  (when-let* ((chunk-id (read-tag stream))
              (chunk-size (read-uint stream 4))
              (chunk-data (if (string= chunk-id "RIFF")
                              (read-tag stream)
                              (funcall parser stream chunk-id chunk-size chunks))))
    (list :chunk-id chunk-id
          :chunk-size chunk-size
          :chunk-data chunk-data)))

(defun read-chunks (stream &optional (parser #'default-parser))
  "Reads every RIFF chunk in a file using the specified parsing function."
  (loop for chunk = (read-chunk stream parser chunks)
     if chunk collect chunk into chunks
     else return chunks))

(defun default-printer (stream chunk-id chunk-size chunk-data chunks)
  "Prints a chunk by printing each byte of data as an unsigned integer."
  (loop for b in chunk-data do (write-uint stream b 1)))

(defun write-chunk (stream printer chunk chunks)
  "Writes a single RIFF chunk using the specified printing function."
  (let ((chunk-id (getf chunk :chunk-id))
        (chunk-size (getf chunk :chunk-size))
        (chunk-data (getf chunk :chunk-data)))
    (write-tag stream chunk-id)
    (write-uint stream chunk-size 4)
    (if (string= chunk-id "RIFF")
        (write-tag stream chunk-data)
        (funcall printer stream chunk-id chunk-size chunk-data chunks))))

(defun write-chunks (stream chunks &optional (printer #'default-printer))
  "Reads every RIFF chunk in a file using the specified printing function."
  (loop for chunk in chunks do
       (write-chunk stream printer chunk chunks)))
