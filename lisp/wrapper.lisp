;;; wrapper.lisp --- Higher level wrapper implementation for libmodbus

;; File:        wrapper.lisp
;; Description: Higher level wrapper implementation for libmodbus
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2024, 凉凉, all rights reserved
;; Created: 2024-11-12 20:54
;; Version: 0.0.0
;; Last-Updated: 2024-11-13 21:22
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

(defmacro to-sec-usec ((time sec usec) &body body)
  "Split time (sec, float) into sec (int) and usec (int). "
  `(let* ((,usec (mod (truncate (* ,time 1000000)) 1000000))
          (,sec  (truncate ,time)))
     ,@body))

(defmethod initialize-instance :after ((ctx context) &key)
  (with-slots (ctx-ptr unit-id byte-timeout response-timeout) ctx
    (when unit-id
      (modbus-set-slave ctx-ptr unit-id))
    (when byte-timeout
      (to-sec-usec (byte-timeout sec usec)
        (modbus-set-byte-timeout ctx-ptr sec usec)))
    (when response-timeout
      (to-sec-usec (byte-timeout sec usec)
        (modbus-set-byte-timeout ctx-ptr sec usec)))))

(defmethod initialize-instance :after ((ctx rtu-context) &key)
  (with-slots (device baud parity data-bits stop-bits) ctx
    (let ((ctx-ptr (modbus-new-rtu device baud parity data-bits stop-bits)))
      (when (cffi:null-pointer-p ctx-ptr)
        (error (make-condition 'modbus-context-error
                               :context-name (format nil "~A" device))))
      (handler-case (assert (/= -1 (modbus-connect ctx-ptr)))
        (t ()
          (modbus-free ctx-ptr)
          (error (make-condition 'modbus-context-error
                                 :context-name (format nil "~A" device)))))
      (setf (slot-value ctx 'ctx-ptr) ctx-ptr)
      (finalize ctx (lambda () (modbus-free ctx-ptr))))))

(defmethod initialize-instance :after ((ctx tcp-context) &key)
  (with-slots (ip port) ctx
    (assert (or (null ip) (stringp ip)))
    (assert (integerp port))
    (let ((ctx-ptr (modbus-new-tcp (or ip (cffi:null-pointer)) port)))
      (when (cffi:null-pointer-p ctx-ptr)
        (error (make-condition 'modbus-context-error
                               :context-name (format nil "~A:~D" ip port))))
      (handler-case (assert (/= -1 (modbus-connect ctx-ptr)))
        (t ()
          (modbus-free ctx-ptr)
          (error (make-condition 'modbus-context-error
                                 :context-name (format nil "~A:~D" ip port)))))
      (setf (slot-value ctx 'ctx-ptr) ctx-ptr)
      (finalize ctx (lambda () (modbus-free ctx-ptr))))))

(defmethod initialize-instance :after ((ctx tcp-pi-context) &key)
  (with-slots (node service) ctx
    (assert (or (null service) (stringp service)))
    (assert (or (null node)    (stringp node)))
    (let ((ctx-ptr (modbus-new-tcp (or node (cffi:null-pointer))
                                   (or service (cffi:null-pointer)))))
      (when (cffi:null-pointer-p ctx-ptr)
        (error (make-condition 'modbus-context-error
                               :context-name (format nil "~A ~D" node service))))
      (handler-case (assert (/= -1 (modbus-connect ctx-ptr)))
        (t ()
          (modbus-free ctx-ptr)
          (error (make-condition 'modbus-context-error
                                 :context-name (format nil "~A ~D" node service)))))
      (setf (slot-value ctx 'ctx-ptr) ctx-ptr)
      (finalize ctx (lambda () (modbus-free ctx-ptr))))))

(defmethod modread ((ctx context) addr len &key (method :read-holding-registers))
  (let ((ctx-ptr (slot-value ctx 'ctx-ptr))
        (buffer  (make-array len))
        (type    (ecase method
                   ((:read-input-status      :read-coil-status)     :uint8)
                   ((:read-holding-registers :read-input-registers) :uint16)))
        (maxr    (ecase method
                   ((:read-input-status      :read-coil-status)     +modbus-max-read-bits+)
                   ((:read-holding-registers :read-input-registers) +modbus-max-read-registers+))))
    (flet ((read-into (addr len offset)
             (let ((buff (cffi:foreign-alloc type :count len)))
               (ecase method
                 (:read-holding-registers (modbus-read-registers       ctx-ptr addr len buff))
                 (:read-input-registers   (modbus-read-input-registers ctx-ptr addr len buff))
                 (:read-input-status      (modbus-read-input-bits      ctx-ptr addr len buff))
                 (:read-coil-status       (modbus-read-bits            ctx-ptr addr len buff)))
               (cffi:foreign-array-to-lisp buff `(:array ,type ,len)
                                           :displaced-to buffer
                                           :displaced-index-offset offset)
               (cffi:foreign-free buff))))
      (loop with offset = 0
            for rlen = (max len maxr)
            while (> len 0)
            do (read-into addr rlen offset)
            do (incf addr   rlen)
            do (decf len    rlen)
            do (incf offset rlen)))))

(defmethod modwrite ((ctx context) addr (buffer array) &key (method :preset-multiple-registers))
  (let ((ctx-ptr (slot-value ctx 'ctx-ptr))
        (len     (length buffer))
        (type    (ecase method
                   (:preset-multiple-registers :uint16)
                   (:force-multiple-coils      :uint8)))
        (maxr    (ecase method
                   (:preset-multiple-registers +modbus-max-write-registers+)
                   (:force-multiple-coils      +modbus-max-write-bits+))))
    (flet ((dump-into (addr len offset)
             (cffi:with-foreign-array (buff (subseq buffer offset (+ offset len)) `(:array ,type))
               (ecase method
                 (:preset-multiple-registers (modbus-write-registers ctx-ptr addr len buff))
                 (:force-multiple-registers  (modbus-write-bits      ctx-ptr addr len buff))))))
      (loop with offset = 0
            for rlen = (max len maxr)
            while (> len 0)
            do (dump-into addr rlen offset)
            do (incf addr   rlen)
            do (decf len    rlen)
            do (incf offset rlen)))))

(defmethod modwrite ((ctx context) addr (value integer) &key (method :preset-single-register))
  (let ((ctx-ptr (slot-value ctx 'ctx-ptr)))
    (ecase method
      (:force-single-coil      (modbus-write-bit      ctx-ptr addr value))
      (:preset-single-register (modbus-write-register ctx-ptr addr value)))))

(defmethod modclose ((ctx context))
  (with-slots (ctx-ptr) ctx
    (modbus-close ctx-ptr)))

;;; wrapper.lisp ends here
