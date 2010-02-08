;;; Library for handling dates in a PLIST format,
;;; like (:year 2009 :day 1 :month 3)
;;; Also provides a couple of utility time functions
;;; (c) Vsevolod Dyomkin <vseloved@gmail.com>. LICENSE: MIT

;;; NB: The library works properly only between years 1901 and 2099,
;;; because leap-year is calculated in a simplified manner

(defpackage :dates
  (:use :common-lisp :cl-ppcre :rutils.usr)
  (:export :*day-scanner*
           :*month-scanner*
           :*year-scanner*

           :parse-date
           :print-date
           :print-numeric-date

           :date=
           :y-m=
           :date<
           :date<=
           :date>
           :date>=
           :datep

           :day
           :day/w
           :week
           :month
           :year
           :year-month
           :year-month-d0

           :day+
           :day/w+
           :week+
           :month+
           :year+

           :dow
           :moy
           :ymd

           :days-in-month
           :next-month-end

           :adjust-date
           :universal-to-unix-time
           :unix-to-universal-time

           :today))

(in-package :dates)

(declaim (inline date< date<= date> date>= print-date
                 day month year year-month year-month-d0
                 day/w week dow moy today ymd
                 leap-year-p beg-of-month month->str
                 str->month days-in-month))

;;; scanners

(defvar *day-scanner*   (cl-ppcre:create-scanner "\\b\\d{1,2}\\b"))
(defvar *month-scanner* (cl-ppcre:create-scanner "\\b[a-zа-я]{3}\\b" :case-insensitive-mode t))
(defvar *year-scanner*  (cl-ppcre:create-scanner "\\d{4}"))

;;; names

(defconstant =01-Jan-1901= 1)  ; Tuesday
;; the lib will correctly calculate day-of-the-week (day/w) in the time
;; timeframe from 01-Jan-1901 to 31-Dec-2099

(defparameter *dow-dict*
  '(:en #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
    :ru #("пн" "вт" "ср" "чт" "пт" "сб" "вс")
    :de #("Mon" "Die" "Mit" "Don" "Fri" "Son" "Sam")))

(defparameter *moy-dict*
  '(:en #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
    :ru #("Январь" "Февраль" "Март" "Апрель" "Май" "Июнь" "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь")
    :de #("Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "October" "November" "Dezember")))

(defparameter *month-dict*
  '(:en #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
    :ru #("янв" "фев" "мар" "апр" "май" "июн" "июл" "авг" "сен" "окт" "ноя" "дек")
    :de #("jan" "feb" "mär" "apr" "mai" "jun" "jul" "aug" "sep" "okt" "nov" "dez")))

(defvar *days-in-month*
  #(31 28 31 30 31 30 31 31 30 31 30 31))


;;; testing

(defun datep (str-or-lst)
  "True if STR-OR-LST is either in string date format D(D)-MMM-YYYY ~
or plist date format (:day d :month m :year y)"
  (gcase ((type-of str-or-lst) :test #'subtypep)
    ('string (and (cl-ppcre:scan *day-scanner* str-or-lst)
                  (cl-ppcre:scan *month-scanner* str-or-lst)
                  (cl-ppcre:scan *year-scanner* str-or-lst)))
    ('list (and (plistp str-or-lst)
                (getf str-or-lst :day)
                (getf str-or-lst :month)
                (getf str-or-lst :year)))))

(defun leap-year-p (date)
  (= (nth-value 1 (floor (year date)
                         4))
     0))


;;; conversion

(defun parse-date (string &optional (lang :en))
  "Parsing the string with date, holding year (exactly 4 digits), ~
month (exactly 3 letters), day (exactly 2 digits) in any order, ~
separated with any separator with possible omition of any of the ~
following. If some of the values are omitted, they are substituted ~
with current ones"
  (multiple-value-bind
        (ignored1 ignored2 ignored3 current-day current-month current-year)
        (get-decoded-time)
    (list :year (or (parse-integer
                     (strcat (cl-ppcre:scan-to-strings *year-scanner* string)
                             " ")
                     :junk-allowed t)
                    current-year)
          :month (or (str->month (cl-ppcre:scan-to-strings *month-scanner*
                                                           string)
                                 lang)
                     current-month)
          :day (or (parse-integer
                    (strcat (cl-ppcre:scan-to-strings *day-scanner*
                                                      string)
                            " ")
                    :junk-allowed t)
                   current-day))))

(defun print-date (date &optional (lang :en) stream)
  (when date
    (format stream "~4d~@[-~3a~]~@[-~2,'0d~]"
            (getf date :year)
            (month->str (getf date :month) lang)
            (getf date :day))))

(defun print-numeric-date (date &optional stream)
  (when date
    (format stream "~4d~2,'0d~2,'0d"
            (getf date :year)
            (getf date :month)
            (getf date :day))))

(defun str->month (str &optional (lang :en))
  "Converting month in the form of 3-char string to number"
  (when str
    (dotimes (i 12)
      (when (equalp str (aref (getf *month-dict* lang) i))
        (return-from str->month (1+ i))))
    (error "Wrong month name: ~a -- in language: ~a"
           str lang)))

(defun month->str (num &optional (lang :en))
  "Converting month in the form of number to 3-char string"
  (when num
    (aref (getf *month-dict* lang) (1- num))))


;;; date comparison

(defun date= (date1 date2 &rest dates)
  (apply #'equalp date1 date2 dates))

(defun y-m= (date1 date2 &rest dates)
  "Only check year and month"
  (and (apply #'= (year date1)  (year date2)  (mapcar #'year dates))
       (apply #'= (month date1) (month date2) (mapcar #'month dates))))

(defun date< (date1 date2)
  (cond
    ((< (getf date1 :year) (getf date2 :year))
     t)
    ((= (getf date1 :year) (getf date2 :year))
     (cond ((< (getf date1 :month) (getf date2 :month))
            t)
           ((= (getf date1 :month) (getf date2 :month))
            (< (getf date1 :day) (getf date2 :day)))))))

(defun date<= (date1 date2)
  (or (date< date1 date2) (date= date1 date2)))

(defun date> (date1 date2)
  (date< date2 date1))

(defun date>= (date1 date2)
  (date<= date2 date1))


;;; accessors

(defun day (date)
  (getf date :day))

(defun month (date)
  (getf date :month))

(defun year (date)
  (getf date :year))

(defun year-month (date)
  (list :year (year date) :month (month date)))

(defun year-month-d0 (date)
  (list :year (year date) :month (month date) :day 0))

(defun today ()
  (parse-date ""))

(defsetf day (date) (new-day)
  `(setf (getf ,date :day) ,new-day))

(defsetf month (date) (new-month)
  `(setf (getf ,date :month) ,new-month))

(defsetf year (date) (new-year)
  `(setf (getf ,date :year) ,new-year))


;; adders

(defun day+ (date num)
  (let* ((days-elapsed (+ (day date)
                          (beg-of-month date)))
         (raw-day (+ days-elapsed num))
         (days-in-year (if (leap-year-p date) 366 365))
         (year (year date)))
    (cond
      ((<= raw-day 0) (day+ `(:year ,(1- year) :month 12 :day 31)
                            (+ num days-elapsed)))
      ((> raw-day days-in-year) (day+ `(:year ,(1+ year) :month 1 :day 1)
                                      (- raw-day days-in-year 1)))
      (t (let ((month (loop for i to 11
                         with next-beg-of-month = 0
                         do (incf next-beg-of-month (aref *days-in-month* i))
                         when (and (leap-year-p date) (= i 1))
                         do (incf next-beg-of-month) ; leap-year
                         when (<= raw-day next-beg-of-month)
                         do (return (1+ i)))))
           (list :year year
                 :month month
                 :day (- raw-day
                         (beg-of-month `(:year ,year :month ,month)))))))))

(defun month+ (date num)
  "Try to return exactly the same day NUM months ahead (or behind). ~
If it's impossible, because of different month ending dates 28..31, ~
give that same date, but in an appropriate month (so 31-apr becomes 1-may etc)"
  (multiple-value-bind (year-add raw-month) (floor (+ (month date) num -1) 12)
    (let ((year (+ (year date) year-add)))
      (nconc (list :year year)
             (let ((raw-day (day date))
                   (max-day (+ (aref *days-in-month* raw-month)
                               (if (leap-year-p `(:year ,year)) 1 0))))
               (if (> raw-day max-day)
                   (list :month (+ raw-month 2)
                         :day (- raw-day max-day))
                   (list :month (1+ raw-month)
                         :day raw-day)))))))

(defun year+ (date num)
  "Try to return exactly the same day & month NUM years ahead (or behind). ~
For the case of 29-feb give 1-mar, if the year isn't a leap one"
  (let ((new-year (+ (year date) num))
        (month (month date))
        (day (day date)))
    (nconc (list :year new-year)
           (if (and (not (leap-year-p `(:year ,new-year)))
                    (= day 29)
                    (= month 2))
               (list :month 3 :day 1)
               (list :month month :day day)))))

(defun week+ (date num) 
  (day+ date (* 7 num)))


;;; week specific

(defun beg-of-year/w (date)
  "Return day-of-week number (starting from monday=0) of the ~
1st of January of the current year"
  (let ((years-elapsed (- (year date) 1901))) 
    (nth-value 1 (floor (+ (* years-elapsed 365)
                           (floor years-elapsed 4)
                           =01-Jan-1901=)
                        7))))

(defun beg-of-month (date)
  "Return number of days elapsed from the beginning of year, ~
until the beginning of the month, current to the given DATE"
  (let ((month (1- (month date))))
    (+ (reduce #'+ *days-in-month* :end month)
       (if (and (> month 1) (leap-year-p date)) 1 0))))

(defun day/w (date)
  "Return day of a week (number)"
  (nth-value 1 (week date)))

#+ nil  ;; alternative
(defun day/w (date)
  (nth 6 (multiple-valuevalue-list
          (decode-universal-time
           (encode-universal-time 0 0 0 (day date) (month date) (year date))))))

(defun week (date)
  "Return number of the week, current to the given DATE. (Numbering ~
starts from 1), and a day of the week (in number format) of this DATE"
  (multiple-value-bind (week day/w)
      (floor (+ (day date)
                (1- (beg-of-month date))
                (beg-of-year/w date))
             7)
    (values (1+ week) day/w)))

(defun dow (date &optional (lang :en))
  "Return day of a week"
  (aref (getf *dow-dict* lang)
        (day/w date)))

(defun moy (date &optional (lang :en))
  "Return month of a year"
  (aref (getf *moy-dict* lang)
        (1- (month date))))

(defun ymd (date)
  "Return year-month-day of the DATE as multiple values"
  (values (year date)
          (month date)
          (day date)))


;;; month specific

(defun next-month-end (&optional (tz-shift 0))
  "return the end of next month, relatively to today, in the timezone, ~
specified by its TZ-SHIFT"
  (let ((next-month (month+ (adjust-date (today) tz-shift) 1)))
    (setf (day next-month) (elt *days-in-month* (1- (month next-month))))
    next-month))

(defun days-in-month (date)
  (elt *days-in-month* (1- (dates:month date))))


;;; time functions

(defun adjust-date (date tz-shift)
  "Adjust DATE according to TZ-SHIFT (+/- 1 day)"
  (if (= tz-shift 0)
      date
      (let* ((decoded-time (multiple-value-list (get-decoded-time)))
             (shift (+ tz-shift
                       (last1 decoded-time)))
             (client-hour (+ (caddr decoded-time) shift)))
        (cond
          ((< client-hour 0) (day+ date -1))
          ((> client-hour 23) (day+ date 1))
          (t date)))))

(defun universal-to-unix-time (universal-time)
  (- universal-time 2208988800))

(defun unix-to-universal-time (unix-time)
  (+ unix-time 2208988800))

;;; end