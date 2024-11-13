;;; package.lisp --- Package definition for CL-LIBMODBUS.

;; File:        package.lisp
;; Description: Package definition for CL-LIBMODBUS.
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 15:13
;; Version: 0.0.0
;; Last-Updated: 2024-11-13 21:21
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

(uiop:define-package #:cl-libmodbus.lowlevel
  (:use)
  (:export
   ;; (ryo.macros:fmt!
   ;;   "~{#:~(~A~)~^~%~}"
   ;;   (mapcar #'car cl-libmodbus.lowlevel::+constant-names+))
   #:+modbus-rtu-rts-down+
   #:+modbus-rtu-rts-up+
   #:+modbus-rtu-rts-none+
   #:+modbus-rtu-rs232+
   #:+modbus-rtu-rs485+
   #:+modbus-tcp-max-adu-length+
   #:+modbus-tcp-slave+
   #:+modbus-tcp-default-port+
   #:+modbus-rtu-max-adu-length+
   #:+modbus-fc-read-coils+
   #:+modbus-fc-read-discrete-inputs+

   #:+modbus-fc-write-single-coil+
   #:+modbus-fc-read-holding-registers+
   #:+modbus-fc-read-input-registers+

   #:+libmodbus-version-minor+
   #:+libmodbus-version-major+
   #:+libmodbus-version-hex+
   #:+modbus-max-pdu-length+
   #:+libmodbus-version-micro+
   #:+modbus-max-wr-write-registers+
   #:+modbus-max-write-registers+
   #:+modbus-max-write-bits+
   #:+modbus-max-wr-read-registers+
   #:+modbus-max-read-bits+
   #:+modbus-broadcast-address+
   #:+modbus-max-read-registers+
   #:+modbus-fc-mask-write-register+
   #:+modbus-fc-report-slave-id+
   #:+modbus-fc-write-and-read-registers+
   #:+modbus-fc-read-exception-status+
   #:+modbus-fc-write-single-register+
   #:+modbus-fc-write-multiple-registers+
   #:+modbus-fc-write-multiple-coils+
   #:+embmdata+
   #:+embxmempar+
   #:+modbus-max-adu-length+
   #:+modbus-enobase+
   #:+embbadexc+
   #:+embbadcrc+
   #:+embbaddata+
   #:+embbadslave+
   #:+embxgtar+
   #:+embxnack+
   #:+embunkexc+
   #:+embxack+
   #:+embxsbusy+
   #:+embxsfail+
   #:+embxgpath+
   #:+embxilfun+
   #:+embxiladd+
   #:+embxilval+

   ;; (ryo.macros:fmt!
   ;;   "~{#:~(~A~)~^~%~}"
   ;;   (mapcar #'car cl-libmodbus.lowlevel::+function-names+))
   #:modbus-tcp-pi-accept
   #:modbus-tcp-pi-listen
   #:modbus-new-tcp-pi
   #:modbus-tcp-accept
   #:modbus-tcp-listen
   #:modbus-new-tcp
   #:modbus-rtu-get-rts-delay
   #:modbus-rtu-set-rts-delay
   #:modbus-rtu-set-custom-rts
   #:modbus-rtu-get-rts
   #:modbus-rtu-set-rts
   #:modbus-rtu-get-serial-mode
   #:modbus-rtu-set-serial-mode
   #:modbus-new-rtu
   #:modbus-set-float-cdab
   #:modbus-set-float-badc
   #:modbus-set-float-dcba
   #:modbus-set-float-abcd
   #:modbus-set-float
   #:modbus-get-float-cdab
   #:modbus-get-float-badc
   #:modbus-get-float-dcba
   #:modbus-get-float-abcd
   #:modbus-get-float
   #:modbus-get-byte-from-bits
   #:modbus-set-bits-from-bytes
   #:modbus-set-bits-from-byte
   #:modbus-disable-quirks
   #:modbus-enable-quirks
   #:modbus-reply-exception
   #:modbus-reply
   #:modbus-receive-confirmation
   #:modbus-receive
   #:modbus-send-raw-request-tid
   #:modbus-send-raw-request
   #:modbus-mapping-free
   #:modbus-mapping-new
   #:modbus-mapping-new-start-address
   #:modbus-report-slave-id
   #:modbus-write-and-read-registers
   #:modbus-mask-write-register
   #:modbus-write-registers
   #:modbus-write-bits
   #:modbus-write-register
   #:modbus-write-bit
   #:modbus-read-input-registers
   #:modbus-read-registers
   #:modbus-read-input-bits
   #:modbus-read-bits
   #:modbus-strerror
   #:modbus-set-debug
   #:modbus-flush
   #:modbus-free
   #:modbus-close
   #:modbus-connect
   #:modbus-get-header-length
   #:modbus-set-indication-timeout
   #:modbus-get-indication-timeout
   #:modbus-set-byte-timeout
   #:modbus-get-byte-timeout
   #:modbus-set-response-timeout
   #:modbus-get-response-timeout
   #:modbus-get-socket
   #:modbus-set-socket
   #:modbus-set-error-recovery
   #:modbus-get-slave
   #:modbus-set-slave))

(defpackage #:cl-libmodbus
  (:use :cl :cl-libmodbus.lowlevel)
  (:import-from :trivial-garbage
   :finalize)
  (:export
   #:rtu-context
   #:tcp-context
   #:tcp-pi-context
   #:modread
   #:modwrite
   #:modclose))

;;; package.lisp ends here
