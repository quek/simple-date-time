(in-package #:simple-date-time)

(defclass duration ()
  ((year
    :initarg :year
    :initform 0
    :accessor year-of)
   (month
    :initarg :month
    :initform 0
    :accessor month-of)
   (day
    :initarg :day
    :initform 0
    :accessor day-of)
   (hour
    :initarg :hour
    :initform 0
    :accessor hour-of)
   (minute
    :initarg :minute
    :initform 0
    :accessor minute-of)
   (second
    :initarg :second
    :initform 0
    :accessor second-of)
   (millisecond
    :initarg :millisecond
    :initform 0
    :accessor millisecond-of)))
