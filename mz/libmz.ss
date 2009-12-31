#lang scheme/base
(require scheme/foreign)
(require "mangle.ss")
(unsafe!)

(define libmz
  (case (system-type)
    [(unix)
     (ffi-lib "libusb")]))

libmz
