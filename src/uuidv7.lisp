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
  (:use #:alexandria
        #:cl
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
    (dotimes (i byte-count)
      (setf (aref bytes i)
            (parse-integer (subseq cleaned-string (* 2 i) (* 2 (1+ i))) :radix 16)))
    bytes))


;; Internal helper functions and constants (not exposed to the user).
(alexandria:define-constant +version+ #*0111
  :test #'equal
  :documentation "bit vector constant for the UUID version (7 in binary)")
(alexandria:define-constant +variant+ #*10
  :test #'equal
  :documentation "bit vector constant for the RFC4122 variant field")
(alexandria:define-constant +bit-vector-length+ 128
  :test #'equal
  :documentation "bit vector length for UUIDv7 values in bits")
(alexandria:define-constant +byte-vector-length+ 16
  :test #'equal
  :documentation "byte vector length for UUIDv7 values in bytes")
(alexandria:define-constant +timestamp-bit-length+ 48
  :test #'equal
  :documentation "bit vector length for the timestamp in milliseconds")
(alexandria:define-constant +uuidv7-string-length+ 36
  :test #'equal
  :documentation "length of UUIDv7 string")

(defun unix-epoch-in-ms ()
  "Get the unix epoch timestamp in milliseconds"
  (let ((time (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix time))  ; Current epoch time is in seconds
       (local-time:timestamp-millisecond time))))    ; This adds millisecond precision

(defun generate-random (n)
  "Generates `n` bits worth of random bytes, returned as a bit vector."
  (let ((bits (make-array n :element-type 'bit))
        (bytes (generate-random-bytes (round-up-to-closest-bytes n))))
    (loop for i below n
          do (setf (aref bits i)
                   (if (logbitp (mod i 8)
                                (aref bytes (floor i 8)))
                       1 0)))
    bits))

(defun round-up-to-closest-bytes (n)
  (ceiling n 8))

(defun generate-random-bytes (n)
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (loop for i below n
          do (setf (aref bytes i) (random 256)))
    bytes))

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
    (dotimes (i +timestamp-bit-length+)
      (setf (aref bits (- 47 i)) (if (logbitp i timestamp) 1 0)))
    bits))

(defun bits->bytes (bits)
  "Converts a 128 bit vector to a 16 byte vector."
  (let ((bytes (make-array +byte-vector-length+ :element-type '(unsigned-byte 8))))
    (dotimes (i +byte-vector-length+)
      (setf (aref bytes i)
            (let* ((start (* i 8))
                   (end (+ start 8)))
              (bits->int (subseq bits start end)))))
    bytes))

(defun bits->int (bits)
  "Convert a simple-bit-vector into an integer."
  (let ((end (- (length bits) 1))
        (reversed (reverse bits)))
    (loop for i below end
          sum (if (logbitp 0 (aref reversed i))
                  (ash 1 i)
                  0))))
