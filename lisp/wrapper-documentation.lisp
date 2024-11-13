;;; wrapper-documentation.lisp --- Documentation on cl-libmodbus wrapper

;; File:        wrapper-documentation.lisp
;; Description: Documentation on cl-libmodbus wrapper
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 21:28
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

(in-package :cl-libmodbus)

(define-condition modbus-context-error (error)
  ((context-name :initarg :context-name
                 :reader  modbus-context-error-context-name)
   (error-name   :initarg  :error-name
                 :initform #+sbcl(modbus-strerror (sb-alien:get-errno))
                           #+(not sbcl)"Modbus Error"
                 :reader   modbus-context-error-error-name))
  (:report (lambda (condition stream)
             (format stream "~A with libmodbus context ~A"
                     (modbus-context-error-error-name   condition)
                     (modbus-context-error-context-name condition)))))

(defclass context ()
  (ctx-ptr
   (unit-id          :initform nil :initarg :unit-id)
   (byte-timeout     :initform nil :initarg :byte-timeout)
   (response-timeout :initform nil :initarg :response-timeout))
  (:documentation
   "The Modbus protocol supports several transport protocols
(eg. serial RTU, Ethernet TCP) called backends in libmodbus.

The first step is to allocate and set a `context' according
to the required backend (RTU or TCP) with a dedicated function. "))

(defclass rtu-context (context)
  ((device    :initform (error "Missing `device'") :initarg :device)
   (baud      :initform 9600                       :initarg :baud)
   (parity    :initform :none                      :initarg :partity)
   (data-bits :initform 8                          :initarg :data-bits)
   (stop-bits :initform 1                          :initarg :stop-bits))
  (:documentation
   "The RTU backend (Remote Terminal Unit) is used in serial
communication and makes use of a compact, binary representation
of the data for protocol communication. The RTU format follows
the commands/data with a cyclic redundancy check checksum as an
error check mechanism to ensure the reliability of data. Modbus
RTU is the most common implementation available for Modbus. A
Modbus RTU message must be transmitted continuously without
inter-character hesitations (extract from Wikipedia, Modbus as
of Mar. 13, 2011, 20:51 GMT).

The Modbus RTU framing calls a slave, a device/service which
handle Modbus requests, and a master, a client which send
requests. The communication is always initiated by the master. "))

(defclass tcp-context (context)
  ((ip   :initform nil                       :initarg :ip)
   (port :initform +modbus-tcp-default-port+ :initarg :port))
  (:documentation
   "The TCP backend implements a Modbus variant used for
communications over TCP/IPv4 networks. It does not require a
checksum calculation as lower layer takes care of the same."))

(defclass tcp-pi-context (context)
  ((node    :initform nil :initarg :node)
   (service :initform nil :initarg :service))
  (:documentation
   "The TCP PI (Protocol Independent) backend implements a
Modbus variant used for communications over TCP IPv4 and IPv6
networks. It does not require a checksum calculation as lower
layer takes care of the same."))

(defgeneric modread (context addr len &key method)
  (:documentation
   "Read registers starting from `addr' of `len' from the `context'.
Return a buffer array as recieved registers value.

Para:
 + `method': can be:
   + `:read-holding-registers' (0x03) < default
   + `:read-coil-status'       (0x01)
   + `:read-input-status'      (0x02)
   + `:read-input-registers'   (0x04)
"))

(defgeneric modwrite (context addr buffer)
  (:documentation
   "Write registers starting to `addr' in the `context'.
The `buffer' should be as the writting buffer. "))

(defgeneric modclose (context)
  (:documentation
   "Close the connection. (Not recommanded to use)"))

;;; wrapper-documentation.lisp ends here
