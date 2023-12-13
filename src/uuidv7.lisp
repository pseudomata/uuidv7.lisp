;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;;  0                   1                   2                   3
;;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |                           unix_ts_ms                          |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |          unix_ts_ms           |  ver  |       rand_a          |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |var|                        rand_b                             |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; |                            rand_b                             |
;;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

(defpackage #:uuidv7
  (:use #:cl
        #:local-time)
  (:export #:generate
           #:bytes->string
           #:string->bytes))

(in-package #:uuidv7)


(defun generate ()
  "Generate and return a UUIDv7 as a byte vector (16 bytes)."
  (let ((unix-ts-ms (timestamp->bits (unix-epoch-in-ms)))
        (rand-a (generate-random 12))
        (rand-b (generate-random 62)))
    (bits->bytes (concat-bits unix-ts-ms
                              +version+
                              rand-a
                              +variant+
                              rand-b))))

(defun bytes->string (bytes)
  "Returns a formatted string from raw UUIDv7 bytes."
  (let ((first (subseq-to-string bytes 0 4))
        (second (subseq-to-string bytes 4 6))
        (third (subseq-to-string bytes 6 8))
        (fourth (subseq-to-string bytes 8 10))
        (fifth (subseq-to-string bytes 10 16)))
    (format nil "~a-~a-~a-~a-~a" first second third fourth fifth)))

(defun string->bytes (string)
  "Returns raw UUIDv7 bytes from a string."
  (let* ((cleaned-string (remove #\- string :test #'char=))
         (byte-count (/ (length cleaned-string) 2))
         (bytes (make-array byte-count :element-type '(unsigned-byte 8))))
    (dotimes (index byte-count)
      (setf (aref bytes index)
            (parse-integer (subseq cleaned-string (* 2 index) (* 2 (1+ index))) :radix 16)))
    bytes))


;; Internal helper functions and constants (not exposed to the user).

(defconstant +version+ #*0111)  ; bit vector holding the binary representation for 7 (the UUID version)
(defconstant +variant+ #*10)    ; bit vector for the RFC4122 variant field

(defconstant +bit-vector-length+ 128)    ; UUIDv7 values are 128 bits
(defconstant +byte-vector-length+ 16)    ; and 16 bytes (128/8)
(defconstant +timestamp-bit-length+ 48)  ; The timestamp in milliseconds is 48 bits
(defconstant +uuidv7-string-length+ 36)  ; UUIDv7 strings are 36 characters

(defun unix-epoch-in-ms ()
  "Get the unix epoch timestamp in milliseconds"
  (let ((time (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix time))  ; Current epoch time is in seconds
       (local-time:timestamp-millisecond time))))    ; This adds millisecond precision

(defun generate-random (n)
  "Generates `n` bits worth of random bytes, returned as a bit vector."
  (let ((bits (make-array n :element-type 'bit)))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (loop for index below n
            do (setf (aref bits index)
                     (if (logbitp 0 (read-byte urandom)) 1 0))))
    bits))

(defun subseq-to-string (array start end)
  "Given an array of bytes, a start, and an end, returns a string containing those bytes."
  (apply #'concatenate
         'string
         (map 'list
              (lambda (x) (format nil "~2,'0X" x))
              (subseq array start end))))

(defun concat-bits (&rest vectors)
  "Concatenate multiple bit vectors into a unified bit vector."
  (apply #'concatenate
         'simple-bit-vector
         (mapcar (lambda (x) (coerce x 'simple-bit-vector))
                 vectors)))

(defun timestamp->bits (timestamp)
  "Returns the epoch timestamp as a 48 bit vector."
  (let ((bits (make-array +timestamp-bit-length+
                           :element-type 'bit
                           :initial-element 0)))
    (dotimes (index +timestamp-bit-length+)
      (setf (aref bits (- 47 index)) (if (logbitp index timestamp) 1 0)))
    bits))

(defun bits->bytes (bits)
  "Converts a 128 bit vector to a 16 byte vector."
  (let ((bytes (make-array +byte-vector-length+ :element-type '(unsigned-byte 8))))
    (dotimes (index +byte-vector-length+)
      (setf (aref bytes index)
            (let* ((start (* index 8))
                   (end (+ start 8)))
              (bits->int (subseq bits start end)))))
    bytes))

(defun bits->int (bits)
  "Convert a simple-bit-vector into an integer."
  (let ((end (- (length bits) 1))
        (reversed (reverse bits)))
    (loop for i from 0 to end
          sum (if (logbitp 0 (aref reversed i))
                  (ash 1 i)
                  0))))
