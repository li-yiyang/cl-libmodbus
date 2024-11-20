;;; cl-libmodbus.asd --- ASDF system definition for CL-LIBMODBUS.

;; File:        cl-libmodbus.asd
;; Description: ASDF system definition for CL-LIBMODBUS.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 15:10
;; Version: 0.0.0
;; Last-Updated: 2024-11-20 17:13
;;           By: 凉凉
;; URL: https://li-yiyang.github.io
;; Keywords:
;; Compatibility:
;;
;;

;;; License
;;
;; this package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; this package is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package. If not, see <https://www.gnu.org/licenses/>.

(asdf:defsystem #:cl-libmodbus
  :author ("凉凉")
  :version "0"
  :description "A Common Lisp wrapper for libmodbus. "
  :defsystem-depends-on (cffi/c2ffi)
  :depends-on (cffi trivial-garbage str)
  :serial t
  :pathname "lisp"
  :components
  ((:file "package")

   ;; lowlevel
   (:file "lowlevel")
   (:module spec
    :pathname "spec"
    :components ((:cffi/c2ffi-file "modbus.h"
                  :package #:cl-libmodbus.lowlevel
                  :ffi-name-transformer "cl-libmodbus.lowlevel::ffi-name-transformer"
                  :foreign-library-name "cl-libmodbus.lowlevel::libmodbus"
                  :foreign-library-spec ((:darwin "libmodbus.dylib")
                                         (:unix   "libmodbus.so")
                                         (t (:default "libmodbus")))
                  :exclude-sources     :all
                  :include-sources     ("modbus.*\\.h")
                  :include-definitions ("^modbus_.*"
                                        "uint\\d+_t")
                  :exclude-definitions ())))
   ))


;;; cl-libmodbus.asd ends here
