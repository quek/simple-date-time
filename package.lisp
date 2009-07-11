(defpackage :simple-date-time
    (:use :cl)
  (:nicknames :date-time :dt)
  (:export #:date-time
           #:year-of
           #:month-of
           #:day-of
           #:hour-of
           #:minute-of
           #:second-of
           #:millisecond-of
           #:normalize
           #:serialize
           #:deserialize
           #:weak-of
           #:year+
           #:month+
           #:day+
           #:hour+
           #:minute+
           #:second+
           #:millisecond+
           #:date=
           #:date/=
           #:date<
           #:date<=
           #:date>
           #:date>=
           #:time=
           #:time/=
           #:time<
           #:time<=
           #:time>
           #:time>=
           #:date-time=
           #:date-time/=
           #:date-time<
           #:date-time<=
           #:date-time>
           #:date-time>=
           #:make-date-time
           #:make-date
           #:make-time
           #:from-universal-time
           #:to-universal-time
           #:now
           #:today
           #:tomorrow
           #:yesterday
           #:from-string
           #:yyyy/mm/dd
           #:yyyy-mm-dd
           #:yy/mm/dd
           #:yy-mm-dd
           ))


