(in-package #:simple-date-time)

(defparameter *default-timezone*
  (- (car (last (multiple-value-list (decode-universal-time (get-universal-time))))))
  "Default timezone. GMT is 0. JST is 9. EST is -5.")

(defclass date-time ()
  ((year
    :initarg :year
    :reader year-of)
   (month
    :initarg :month
    :reader month-of)
   (day
    :initarg :day
    :reader day-of)
   (hour
    :initarg :hour
    :reader hour-of)
   (minute
    :initarg :minute
    :reader minute-of)
   (second
    :initarg :second
    :reader second-of)
   (millisecond
    :initarg :millisecond
    :initform 0
    :reader millisecond-of)))

(defmethod initialize-instance :after ((date-time date-time) &rest args)
  (declare (ignore args))
  (let* ((slots '(year month day hour minute second))
         (boundp (mapcar (lambda (slot) (slot-boundp date-time slot))
                         slots)))
    (when (notevery #'identity boundp)
      (multiple-value-bind (se mi ho da mo ye)
          (decode-universal-time (get-universal-time))
        (macrolet ((m (nth slot value)
                     `(unless (nth ,nth boundp)
                        (setf (slot-value date-time ',slot) ,value))))
          (m 0 year ye)
          (m 1 month mo)
          (m 2 day da)
          (m 3 hour ho)
          (m 4 minute mi)
          (m 5 second se))))))

(defmethod print-object ((date-time date-time) stream)
  (print-unreadable-object (date-time stream :type t :identity t)
    (format stream "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d"
            (year-of date-time)
            (month-of date-time)
            (day-of date-time)
            (hour-of date-time)
            (minute-of date-time)
            (second-of date-time)
            (millisecond-of date-time))))

(defvar *normal-year-days-of-month* #(31 28 31 30 31 30 31 31 30 31 30 31))
(defvar *leap-year-days-of-month* #(31 29 31 30 31 30 31 31 30 31 30 31))

(defun clone (date-time)
  (make-instance 'date-time
                 :year (year-of date-time)
                 :month (month-of date-time)
                 :day (day-of date-time)
                 :hour (hour-of date-time)
                 :minute (minute-of date-time)
                 :second (second-of date-time)
                 :millisecond (millisecond-of date-time)))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400))
         t)
        ((zerop (mod year 100))
         nil)
        ((zerop (mod year 4))
         t)
        (t
         nil)))

(defun days-of-year (year)
  (if (leap-year-p year)
      366
      365))

(defun days-of-month (year month)
  (svref (if (leap-year-p year)
             *leap-year-days-of-month*
             *normal-year-days-of-month*)
         (1- month)))

(defun serialize-date (date-time)
  (+ (loop for i from 1 below (year-of date-time)
           sum (days-of-year i))
     (loop for i from 1 below (month-of date-time)
           sum (days-of-month (year-of date-time) i))
     (day-of date-time)
     -1))

(defun serialize-time (date-time)
  (+ (* (hour-of date-time) 60 60 1000)
     (* (minute-of date-time) 60 1000)
     (* (second-of date-time) 1000)
     (millisecond-of date-time)))

(defun serialize (date-time)
  "Calculates integer milliseconds from DATE-TIME object beginning of
time (Jan 1 1 0:0:0)"
  (let ((days (serialize-date date-time)))
    (+ (* days #.(* 24 60 60 1000))
       (serialize-time date-time))))

(defun normalize-time (date-time)
  (macrolet ((f (divisor slot next)
               `(when (or (<= ,divisor (slot-value date-time ',slot))
                          (minusp (slot-value date-time ',slot)))
                  (multiple-value-bind (quotient remainder)
                      (floor (slot-value date-time ',slot) ,divisor)
                    (setf (slot-value date-time ',slot) remainder)
                    (incf (slot-value date-time ',next) quotient)))))
    (f 1000 millisecond second)
    (f 60   second      minute)
    (f 60   minute      hour)
    (f 24   hour        day))
  date-time)


(defun normalize-date (date-time)
  "Returns a DATE-TIME object with date slots adjusted so they are all
within normal bounds. "
  (with-slots (day month year) date-time
    (loop
      if (< month 1)
        do (progn (setf month 12)
                  (decf year))
      else if (< 12 month)
             do (progn (decf month 12)
                       (incf year))
      else if (< day 1)
             do (progn (if (= month 1)
                           (progn
                             (setf month 12)
                             (decf year))
                           (decf month))
                       (setf day (days-of-month year month)))
      else if (< (days-of-month year month) day)
             do (progn (decf day (days-of-month year month))
                       (incf month))
      else do (return date-time))))


(defun normalize (date-time)
  "Returns a DATE-TIME object with date slots adjusted to be within
normal bounds. "
  (normalize-date (normalize-time date-time)))

(defun deserialize (millisecond)
  "Returns a normalized DATE-TIME object from input in milliseconds
from beginning of date-time (1 1 1 0:0:0). "
  (normalize (make-instance 'date-time
                            :year 1
                            :month 1
                            :day 1
                            :hour 0
                            :minute 0
                            :second 0
                            :millisecond millisecond)))

(defun day-of-week-of (date-time)
  (mod (1+ (serialize-date date-time)) 7))

(setf (fdefinition 'week-of) #'day-of-week-of)

(defun day-name-of (date)
  (nth (day-of-week-of date)
       (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))

(defparameter *short-month-names* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                    "Aug" "Sep" "Oct" "Nov" "Dec"))
(defun month-name-of (date)
  (nth (1- (month-of date))
       *short-month-names*))

(defun from-short-month-name (month-name)
  (let ((month (position month-name *short-month-names* :test #'string-equal)))
    (if month
        (1+ month)
        nil)))

(macrolet ((m (x)
             `(defun ,(intern (concatenate 'string (string x) "+"))
                  (date-time delta)
                "Function adds X to DATE-TIME object slot without
normalizing. "
                (normalize
                 (make-instance 'date-time
                                :year (year-of date-time)
                                :month (month-of date-time)
                                :day ,(if (eq x 'day)
                                          `(+ delta (day-of date-time))
                                          `(day-of date-time))
                                :day ,(if (eq x 'day)
                                          `(+ delta (day-of date-time))
                                          `(day-of date-time))
                                :hour ,(if (eq x 'hour)
                                           `(+ delta (hour-of date-time))
                                           `(hour-of date-time))
                                :minute ,(if (eq x 'minute)
                                             `(+ delta (minute-of date-time))
                                             `(minute-of date-time))
                                :second ,(if (eq x 'second)
                                             `(+ delta (second-of date-time))
                                             `(second-of date-time))
                                :millisecond ,(if (eq x 'millisecond)
                                                  `(+ delta (millisecond-of date-time))
                                                  `(millisecond-of date-time)))))))
  (m day)
  (m hour)
  (m minute)
  (m second)
  (m millisecond))

(defun ensure-last-day-of-month (date-time)
  (let ((days (days-of-month (year-of date-time) (month-of date-time))))
    (when (< days (day-of date-time))
      (setf (slot-value date-time 'day) days)))
  date-time)

(defun year+ (date-time delta)
  "Increments YEAR-OF DATE-TIME object by DELTA. Does not normalize
the result. "
  (ensure-last-day-of-month
   (make-instance 'date-time
                  :year (+ (year-of date-time) delta)
                  :month (month-of date-time)
                  :day (day-of date-time)
                  :hour (hour-of date-time)
                  :minute (minute-of date-time)
                  :second (second-of date-time)
                  :millisecond (millisecond-of date-time))))


(defun month+ (date-time delta)
  "Increments MONTH-OF DATE-TIME object by DELTA. Does not normalize
the result. "
  (multiple-value-bind (quotient remainder)
      (floor (+ (month-of date-time) delta) 12)
    (multiple-value-bind (year month)
        (if (zerop remainder)
            (values  (+ (year-of date-time) (1- quotient))
                     12)
            (values (+ (year-of date-time) quotient)
                    remainder))
      (ensure-last-day-of-month
       (make-instance 'date-time
                      :year year
                      :month month
                      :day (day-of date-time)
                      :hour (hour-of date-time)
                      :minute (minute-of date-time)
                      :second (second-of date-time)
                      :millisecond (millisecond-of date-time))))))



(defun date= (dt1 dt2)
  "Compare two DATE-TIME objects for date1 = date2"
  (and (= (year-of dt1) (year-of dt2))
       (= (month-of dt1) (month-of dt2))
       (= (day-of dt1) (day-of dt2))))

(defun date< (dt1 dt2)
  "Compare two DATE-TIME objects for date1 < date2"
  (or (< (year-of dt1) (year-of dt2))
      (and (= (year-of dt1) (year-of dt2))
           (or (< (month-of dt1) (month-of dt2))
               (and (= (month-of dt1) (month-of dt2))
                    (< (day-of dt1) (day-of dt2)))))))

(defun time= (dt1 dt2)
  "Compare two DATE-TIME objects for time1 = time2"
  (and (= (hour-of dt1) (hour-of dt2))
       (= (minute-of dt1) (minute-of dt2))
       (= (second-of dt1) (second-of dt2))
       (= (millisecond-of dt1) (millisecond-of dt2))))

(defun time< (dt1 dt2)
  "Compare two DATE-TIME objects for time1 < time2"
  (or (< (hour-of dt1) (hour-of dt2))
      (and (= (hour-of dt1) (hour-of dt2))
           (or (< (minute-of dt1) (minute-of dt2))
               (and (= (minute-of dt1) (minute-of dt2))
                    (or (< (second-of dt1) (second-of dt2))
                        (and (= (second-of dt1) (second-of dt2))
                             (< (millisecond-of dt1)
                                (millisecond-of dt2)))))))))

(defun date-time= (dt1 dt2)
  "Compare two DATE-TIME objects for both date1 = date2 and time1 =
time2."
  (and (date= dt1 dt2)
       (time= dt1 dt2)))

(defun date-time< (dt1 dt2)
  "Compare two DATE-TIME objects for date1 < date2"
  (or (date< dt1 dt2)
      (and (date= dt1 dt2)
           (time< dt1 dt2))))

(macrolet ((m (x)
             (labels ((sym (&rest args)
                        (intern (apply #'concatenate 'string
                                       (mapcar #'string args)))))
               (let ((name (symbol-name x)))
                 `(progn
                    (defun ,(sym name '/=) (dt1 dt2)
                      (not (,(sym name '=) dt1 dt2)))
                    (defun ,(sym name '<=) (dt1 dt2)
                      (or (,(sym name '=) dt1 dt2)
                          (,(sym name '<) dt1 dt2)))
                    (defun ,(sym name '>) (dt1 dt2)
                      (and (not (,(sym name '=) dt1 dt2))
                           (not (,(sym name '<) dt1 dt2))))
                    (defun ,(sym name '>=) (dt1 dt2)
                      (not (,(sym name '<) dt1 dt2))))))))
  (m date)
  (m time)
  (m date-time))

(defun make-date-time (year month day
                       &optional (hour 0) (minute 0) (second 0)
                       (millisecond 0))
  "Constructs a DATE-TIME object from given date and optional time
arguments."
  (make-instance 'date-time
                 :year year
                 :month month
                 :day day
                 :hour hour
                 :minute minute
                 :second second
                 :millisecond millisecond))

(defun make-date (year month day)
  "Constructs a DATE-TIME object from given date arguments."
  (make-date-time year month day))

(defun make-time (hour minute second &optional (millisecond 0))
  "Constructs a DATE-TIME object from given time arguments."
  (make-date-time 1 1 1 hour minute second millisecond))

(defun from-universal-time (&optional (universal-time (get-universal-time))
                            (millisecond 0))
  "Returns a DATE-TIME object set from UNIVERSAL-TIME (default is now)"
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time universal-time)
    (make-date-time ye mo da ho mi se millisecond)))

(defun to-universal-time (date-time &optional (timezone *default-timezone*))
  "Returns the universal time for DATE-TIME object (truncating
milliseconds). "
  (encode-universal-time (second-of date-time)
                         (minute-of date-time)
                         (hour-of date-time)
                         (day-of date-time)
                         (month-of date-time)
                         (year-of date-time)
                         (- timezone)))




(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun from-posix-time (time)
  "Returns a DATE-TIME object from posix TIME"
  (from-universal-time (+ time +posix-epoch+)))


(defun now ()
  "Returns DATE-TIME object set to the current time."
  (from-universal-time
   (get-universal-time)
   #-sbcl 0
   #+sbcl (floor (cadr (multiple-value-list (sb-ext:get-time-of-day)))
                 1000)))

(defun today ()
  "Returns a DATE-TIME object set to the start of the current day."
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time (get-universal-time))
    (declare (ignore se mi ho))
    (make-date-time ye mo da)))

(defun tomorrow ()
  "Returns a DATE-TIME object set to the start of the day following
the current day."
  (day+ (today) 1))

(defun yesterday ()
  "Returns a DATE-TIME object set to the start of the day before the
current day."
  (day+ (today) -1))


(defun from-string-with-format (string format)
  (declare (ignore string format))
  ;; TODO
  )

(defun from-string (string &key format (timezone *default-timezone*))
  "Returns a DATE-TIME object set from parsing STRING. "
  (if format
      (from-string-with-format string format)
      (cond ((cl-ppcre:scan "^\\d{14}$" string)
             (make-date-time (parse-integer string :start 0 :end 4)
                             (parse-integer string :start 4 :end 6)
                             (parse-integer string :start 6 :end 8)
                             (parse-integer string :start 8 :end 10)
                             (parse-integer string :start 10 :end 12)
                             (parse-integer string :start 12 :end 14)))
            ((ppcre:register-groups-bind
                 ((#'parse-integer day) (#'from-short-month-name month)
                  (#'parse-integer year hour minute second)
                  ((lambda (x) (or (parse-integer x :junk-allowed t) 0)) tz))
                 ("\\S+, (\\d{2}) (.*) (\\d{4}) (\\d{2}):(\\d{2}):(\\d{2}) (GMT|[+-]\\d{4})" string)
               ;; Mon, 09 Sep 2011 23:36:00 GMT
               (hour+ (minute+ (make-date-time year month day hour minute second)
                               (- (+ (* (truncate tz 100) 60) (mod tz 100))))
                      timezone)))
            (t ;; TODO
             nil))))

(defun yyyy/mm/dd (date-time)
  "Write string for  DATE-TIME object in format: yyyy/mm/dd"
  (format nil "~04,'0d/~02,'0d/~02,'0d" (year-of date-time)
          (month-of date-time) (day-of date-time)))

(defun yyyy-mm-dd (date-time)
  "Write string for DATE-TIME object in format: yyyy-mm-dd"
  (format nil "~04,'0d-~02,'0d-~02,'0d" (year-of date-time)
          (month-of date-time) (day-of date-time)))

(defun yy/mm/dd (date-time)
  "Write string for DATE-TIME object in format: yy/mm/dd"
  (format nil "~02,'0d/~02,'0d/~02,'0d" (mod (year-of date-time) 100)
          (month-of date-time) (day-of date-time)))

(defun yy-mm-dd (date-time)
  "Write string for DATE-TIME object in format: yy-mm-dd"
  (format nil "~02,'0d-~02,'0d-~02,'0d" (mod (year-of date-time) 100)
          (month-of date-time) (day-of date-time)))

(defun yy.mm.dd (date-time)
  "Write string for DATE-TIME object in format: yy.mm.dd"
  (format nil "~02,'0d.~02,'0d.~02,'0d" (mod (year-of date-time) 100)
          (month-of date-time) (day-of date-time)))

(defun |hh:mm| (date-time)
  "Write string for DATE-TIME object in format: hh:mm"
  (format nil "~02,'0d:~02,'0d" (hour-of date-time) (minute-of date-time)))

(defun |yyyy/mm/dd hh:mm| (date-time)
  "Write string for DATE-TIME object in format: yyyy/mm/dd hh:mm"
  (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d"
          (year-of date-time) (month-of date-time) (day-of date-time)
          (hour-of date-time) (minute-of date-time)))

(defun |yyyy-mm-dd hh:mm| (date-time)
  "Write string for DATE-TIME object in format: yyyy-mm-dd hh:mm"
  (format nil "~04,'0d-~02,'0d-~02,'0d ~02,'0d:~02,'0d"
          (year-of date-time) (month-of date-time) (day-of date-time)
          (hour-of date-time) (minute-of date-time)))

(defun |yyyy/mm/dd hh:mm:ss| (date-time)
  "Write string for DATE-TIME object in format: yyyy/mm/dd hh:mm:ss"
  (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d"
          (year-of date-time) (month-of date-time) (day-of date-time)
          (hour-of date-time) (minute-of date-time) (second-of date-time)))

(defun |yyyy-mm-dd hh:mm:ss| (date-time)
  "Write string for DATE-TIME object in format: yyyy-mm-dd hh:mm:ss"
  (format nil "~04,'0d-~02,'0d-~02,'0d ~02,'0d:~02,'0d:~02,'0d"
          (year-of date-time) (month-of date-time) (day-of date-time)
          (hour-of date-time) (minute-of date-time) (second-of date-time)))

(defun yyyymmdd (date-time)
  "Write string for DATE-TIME object in format: yyyymmdd"
  (format nil "~04,'0d~02,'0d~02,'0d"
          (year-of date-time) (month-of date-time) (day-of date-time)))

(defun |yyyymmddThhmmssZ| (date-time)
  "Write string for DATE-TIME object in format: YYYYMMDD'T'HHMMSS'Z'"
  ;; TODO tizezone
  (format nil "~04,'0d~02,'0d~02,'0dT~02,'0d~02,'0d~02,'0dZ"
          (year-of date-time) (month-of date-time) (day-of date-time)
          (hour-of date-time) (minute-of date-time) (second-of date-time)))

(defun http-date (date-time &optional (timezone *default-timezone*))
  "Write string for HTTP-Date"
  (let ((date-tz (hour+ date-time (- timezone))))
    (format nil "~a, ~02,'0d ~a ~04,'0d ~02,'0d:~02,'0d:~02,'0d GMT"
	    (day-name-of date-tz)
	    (day-of date-tz)
	    (month-name-of date-tz)
	    (year-of date-tz)
	    (hour-of date-tz)
	    (minute-of date-tz)
	    (second-of date-tz))))

;; For the timezone, as decode-universal-time does not seem to handle
;; minutes, the precision is (integral) hours, but the RFC says minutes
;; should be OK too...
(defun rfc-2822 (date-time &optional (timezone 0))
  "Write string for DATE-TIME object as per rfc-2822 (including time
zone)."
  (let* ((tz (car (last (multiple-value-list
			 (decode-universal-time
			  (to-universal-time date-time))))))
	 (date-tz (hour+ (hour+ date-time tz) timezone)))
    (format nil "~a, ~02,'0d ~a ~04,'0d ~02,'0d:~02,'0d:~02,'0d ~a~02,'0d00"
	    (day-name-of date-tz)
	    (day-of date-tz)
	    (month-name-of date-tz)
	    (year-of date-tz)
	    (hour-of date-tz)
	    (minute-of date-tz)
	    (second-of date-tz)
	    (if (>= timezone 0) "+" "-")
	    (if (>= timezone 0) timezone (- timezone)))))
