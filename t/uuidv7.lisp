;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:uuidv7-test
  (:use #:uuidv7
        #:fiveam))

(in-package #:uuidv7-test)

(def-suite test-uuidv7 :description "Test suite for this UUIDv7 library.")
(in-suite test-uuidv7)

;; (test generate "Test the generate function to generate a UUIDv7."
;;   (is (generate)))

(test to-bytes-and-to-string "Test the to-bytes and to-string function using example from spec."
      (let* ((uuidv7-string "017F22E2-79B0-7CC3-98C4-DC0C0C07398F")
             (uuidv7-bytes (to-bytes uuidv7-string)))
        (is (= uuidv7-string (to-string uuidv7-bytes)))))
