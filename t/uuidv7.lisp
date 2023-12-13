;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:uuidv7-test
  (:use #:cl
        #:uuidv7
        #:fiveam))

(in-package #:uuidv7-test)

(fiveam:def-suite test-uuidv7 :description "Test suite for this UUIDv7 library.")
(fiveam:in-suite test-uuidv7)

(fiveam:test generate-ids
  "Generate a bunch of UUIDv7 values and verify that they are unique and can be converted into
strings, and then back into bytes."
  (defvar ids '())
  (finishes (dotimes (_ 1000)
              (let ((id (generate)))
                (finishes (bytes->string id))
                (finishes (string->bytes (bytes->string id)))
                (push (generate) ids))))
  (let ((number-of-ids (length ids))
        (number-of-ids-without-duplicates (length (remove-duplicates ids))))
    (is (= number-of-ids
           number-of-ids-without-duplicates))))


(fiveam:test string-to-bytes-conversion
  "Test string->bytes and bytes->string functions using example from spec."
  (let* ((uuidv7-string "017F22E2-79B0-7CC3-98C4-DC0C0C07398F")
         (uuidv7-bytes (string->bytes uuidv7-string)))
    (is (string= uuidv7-string (bytes->string uuidv7-bytes)))))

(fiveam:run! 'test-uuidv7)
