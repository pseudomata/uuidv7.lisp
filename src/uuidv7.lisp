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

(defconstant +version+ #*0111)  ; bit vector holding the binary representation for 7 (the UUID version)
(defconstant +variant+ #*10)    ; bit vector for the RFC4122 variant field

(defun generate ()
  "Generate and return a UUIDv7 as a byte array (16 bytes)."
  (let ((unix-ts-ms (ts->bit-vector (unix-epoch-in-ms)))
        (rand-a (generate-random 12))
        (rant-b (generate-random 62)))
    (bit-vector->bytes (concat-bit-vectors '(unix-ts-ms
                                             +version+
                                             rand-a
                                             +variant+
                                             rand-b)))))

(defun bytes->string (bytes)
  "Returns a formatted string from raw UUIDv7 bytes."
  ())

(defun string->bytes (string)
  "Returns raw UUIDv7 bytes from a string."
  ())


;; Internal helper functions (not exposed to the user).

(defun unix-epoch-in-ms ()
  "Get the unix epoch timestamp in milliseconds"
  (let ((time (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix time))  ; Current epoch time is in seconds
       (local-time:timestamp-milliseconds time))))   ; This adds millisecond precision

(defun generate-random (n)
  "Generates `n` bits worth of random bytes, returned as a bit vector."
  (let ((bit-vector (make-sequence '(vector bit) n)))
    (with-open-file (urandom "/dev/urandom" :element-type 'bit)
      (read-sequence bit-vector urandom)
      (bit-vector))))

(defun concat-bit-vectors (&rest vectors)
  "Concatenate multiple bit vectors into a single bit vector."
  (concatenate 'simple-bit-vector vectors))

(defconstant +timestamp-bit-length+ 48)  ; The timestamp in milliseconds is 48 bits

(defun ts->bit-vector (ts)
  "Returns the epoch timestamp as a 48 bit simple-bit-vector."
  (let ((bit-vector (make-array +timestamp-bit-length+
                                :element-type 'bit
                                :initial-element 0)))
    (dotimes (index +timestamp-bit-length+)
      (setf (elt bit-vector (- 47 index)) (logbitp index ts)))
    bit-vector))

(defconstant +bit-vector-length+ 128)  ; UUIDv7 values are 128 bits
(defconstant +byte-array-length+ 16)   ; and 16 bytes (128/8)

(defun bit-vector->bytes (bit-vector)
  "Converts a bit vector to a byte array."
  (map 'vector
       (lambda (index)
         (if (< index +bit-vector-length+)
             (aref bit-vector (- +bit-vector-length+ index 1))
             0))
       (loop repeat +byte-array-length+))))
