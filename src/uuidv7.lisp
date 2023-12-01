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

(in-package #:uuidv7)

(defconstant +version+ #*0111)  ; bit vector holding the binary representation for 7 (the UUID version)
(defconstant +variant+ #*10)    ; bit vector for the RFC4122 variant field

(defun generate ()
  "Returns a newly generated UUIDv7 as raw bytes."
  (print "todo"))

(defun to-string (bytes)
  "Returns a formatted string from raw UUIDv7 bytes."
  (print "todo"))

(defun to-bytes (string)
  "Returns raw UUIDv7 bytes from a string."
  (print "todo"))

(defun generate-random-bytes (n)
  "Generates `n` bits worth of random bytes, returned as raw byte array."
  (let ((bytes (make-array n :element-type 'bit)))
    (with-open-file (urandom "/dev/urandom" :element-type 'bit)
      (read-sequence bytes urandom)
      (bytes))))
