;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(asdf:defsystem #:uuidv7-test
  :author "Pseudomata <pseudomata@proton.me>"
  :license  "MPL-2.0"
  :depends-on (#:uuidv7
               #:fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "uuidv7")))))
