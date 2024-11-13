;;; lowlevel.lisp --- Some preload functions dealing with cl-libmodbus.lowlevel.

;; File:        lowlevel.lisp
;; Description: Some preload functions dealing with cl-libmodbus.lowlevel.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 20:59
;; Version: 0.0.0
;; Last-Updated: 2024-11-12 20:59
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

(cl:in-package :cl-libmodbus.lowlevel)

(cl:defun ffi-name-transformer (name kind cl:&key cl:&allow-other-keys)
  (cl:check-type name cl:string)
  (cl:case kind
    ((:constant :member)
     (cl:assert (cl:not (cl:symbolp name)))
     (cl:format cl:nil "+~A+" (str:upcase (str:param-case name))))
    (cl:t (str:upcase (str:param-case name)))))

;;; lowlevel.lisp ends here
