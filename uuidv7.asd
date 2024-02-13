;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(asdf:defsystem #:uuidv7
  :description "UUIDv7 Implementation."
  :author "Pseudomata <pseudomata@proton.me>"
  :license  "MPL-2.0"
  :version "0.1.0"
  :bug-tracker "https://github.com/pseudomata/uuidv7.lisp/issues"
  :source-control (:git "https://github.com/pseudomata/uuidv7.lisp.git")
  :name "uuidv7"
  :depends-on (#:alexandria
               #:local-time)
  :components ((:module "src"
                :serial t
                :components ((:file "uuidv7"))))
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op #:uuidv7-test))))
