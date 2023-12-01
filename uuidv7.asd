;;;;  This Source Code Form is subject to the terms of the Mozilla Public
;;;;  License, v. 2.0. If a copy of the MPL was not distributed with this
;;;;  file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem uuidv7
  :description "UUIDv7 Implementation."
  :author "Pseudomata <pseudomata@proton.me>"
  :license  "MPL-2.0"
  :version "0.1.0"
  :depends-on (:alexandria
               :local-time)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "uuidv7")))))
