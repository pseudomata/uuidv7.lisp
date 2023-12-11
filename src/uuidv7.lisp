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

(defconstant +bit-vector-length+ 128)    ; UUIDv7 values are 128 bits
(defconstant +byte-vector-length+ 16)    ; and 16 bytes (128/8)
(defconstant +timestamp-bit-length+ 48)  ; The timestamp in milliseconds is 48 bits
(defconstant +uuidv7-string-length+ 36)  ; UUIDv7 strings are 36 characters

(defun generate ()
  "Generate and return a UUIDv7 as a byte vector (16 bytes)."
  (let ((unix-ts-ms (ts->bits (unix-epoch-in-ms)))
        (rand-a (generate-random 12))
        (rand-b (generate-random 62)))
    (bits->bytes (concat-bits '(unix-ts-ms
                                +version+
                                rand-a
                                +variant+
                                rand-b)))))

(defun bytes->string (bytes)
  "Returns a formatted string from raw UUIDv7 bytes."
  (assert (= (length bytes) +byte-vector-length+) "UUIDv7 byte vector should be 16 bytes")
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (logior (ash (aref bytes 0) 40)
                  (ash (aref bytes 1) 32)
                  (ash (aref bytes 2) 24)
                  (ash (aref bytes 3) 16)
                  (ash (aref bytes 4) 8)
                  (aref bytes 5))
          (logior (logand (ash (aref bytes 6) 4) 15)
                  (ash (aref bytes 7) 16))
          (logior (ash (aref bytes 6) 8)
                  (aref bytes 7))
          (logior (logand (ash (aref bytes 7) 6) 3)
                  (ash (aref bytes 7) 18)
                  (ash (aref bytes 8) 10)
                  (ash (aref bytes 9) 2)
                  (logand (ash (aref bytes 9) 62) 1)
                  (ash (aref bytes 10) 54)
                  (ash (aref bytes 11) 46)
                  (ash (aref bytes 12) 38)
                  (ash (aref bytes 13) 30)
                  (ash (aref bytes 14) 22)
                  (ash (aref bytes 15) 14))))

(defun string->bytes (string)
  "Returns raw UUIDv7 bytes from a string."
  (assert (= (length string) +uuidv7-string-length+) "UUIDv7 strings should be 36 characters")
  (let* ((cleaned-string (remove #\- string :test #'char=))
         (byte-count (/ (length cleaned-string) 2))
         (bytes (make-vector byte-count :element-type '(unsigned-byte 8))))
    (dotimes (index byte-count)
      (setf (aref bytes index)
            (parse-integer (subseq cleaned-string (* 2 index) (* 2 (1+ index))) :radix 16)))
    bytes))


;; Internal helper functions (not exposed to the user).

(defun unix-epoch-in-ms ()
  "Get the unix epoch timestamp in milliseconds"
  (let ((time (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix time))  ; Current epoch time is in seconds
       (local-time:timestamp-milliseconds time))))   ; This adds millisecond precision

(defun generate-random (n)
  "Generates `n` bits worth of random bytes, returned as a bit vector."
  (let ((bits (make-vector n :element-type 'bit)))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (loop for index below n
            do (setf (elt bits index)
                     (logbitp 0 (read-byte urandom)))))
    (assert (= (length bits) n) "bits vector should contain `n` bits"
    bits))

(defun concat-bits (&rest vectors)
  "Concatenate multiple bit vectors into a unified bit vector."
  (concatenate 'simple-bit-vector vectors))

(defun ts->bits (ts)
  "Returns the epoch timestamp as a 48 bit vector."
  (let ((bits (make-vector +timestamp-bit-length+
                           :element-type 'bit
                           :initial-element 0)))
    (dotimes (index +timestamp-bit-length+)
      (setf (elt bits (- 47 index)) (logbitp index ts)))
    bits))

(defun bits->bytes (bits)
  "Converts a 128 bit vector to a 16 byte vector."
  (assert (= (length bits) +bit-vector-length+) "bits vector should contain 128 bits")
  (map 'vector
       ;; extracts the nth byte for each index
       (lambda (index) (logior (ash (aref bits (+ index 0)) 7)
                               (ash (aref bits (+ index 1)) 6)
                               (ash (aref bits (+ index 2)) 5)
                               (ash (aref bits (+ index 3)) 4)
                               (ash (aref bits (+ index 4)) 3)
                               (ash (aref bits (+ index 5)) 2)
                               (ash (aref bits (+ index 6)) 1)
                               (aref bits (+ index 7))))
       ;; iterate over 8 bits at a time
       (loop for index from 0 below +bit-vector-length+ by 8)))
