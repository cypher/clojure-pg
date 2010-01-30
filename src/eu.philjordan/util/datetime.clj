(ns eu.philjordan.util.datetime (:refer-clojure))

(defn get-future-years "returns a java.util.Date that is delta-years in the future. -ve years -> past"
	([delta-years date]
		(let
			[cal (new java.util.GregorianCalendar)]
			(. cal (setTime date))
			(. cal (add (. java.util.Calendar YEAR) delta-years))
			(. cal (getTime))))
	([delta-years]
		(get-future-years delta-years (. (new java.util.GregorianCalendar) getTime))))

(defn create-date-fmt [fmt-str]
	(new java.text.SimpleDateFormat fmt-str))

(defn parse-date [fmt date-str]
	(. fmt (parseObject date-str)))
(defn now []
	(. (new java.util.GregorianCalendar) (getTime)))
(defn date-str [fmt date]
	(. fmt (format date)))

(let [simple-fmt (create-date-fmt "yyyy/MM/dd HH:mm:ss")]
	(defn simple-datetime [date-str]
		(parse-date simple-fmt date-str))
	(defn simple-datetime-str [date]
		(date-str simple-fmt date)))

(let [simple-date-fmt (create-date-fmt "yyyy/MM/dd")]
	(defn simple-date [date-str]
		(parse-date simple-date-fmt date-str))
	(defn simple-date-str [date]
		(date-str simple-date-fmt date)))

(let [simple-time-fmt (create-date-fmt "HH:mm:ss")]
	(defn simple-time [time-str]
		(parse-date simple-time-fmt time-str))
	(defn simple-time-str [time]
		(date-str simple-time-fmt time)))

(let
	[field-map
		(hash-map
			:year (. java.util.Calendar YEAR)
			:month (. java.util.Calendar MONTH)
			:week (. java.util.Calendar WEEK_OF_YEAR)
			:day (. java.util.Calendar DAY_OF_MONTH)
			:hour (. java.util.Calendar HOUR)
			:minute (. java.util.Calendar MINUTE)
			:second (. java.util.Calendar SECOND)
			:millisecond (. java.util.Calendar MILLISECOND)
			:weekday (. java.util.Calendar DAY_OF_WEEK))]
	(defn add-date
		([date]
			date)
		([date field delta & tail]
			(add-date date
				(list*
					field delta tail)))
		([date delta-seq]
			(let
				[cal (new java.util.GregorianCalendar)]
				(. cal (setTime date))
				(dorun 
					(map
						(fn [[field delta]]
							(. cal (add (field-map field) delta)))
						(partition 2 delta-seq)))
				(. cal (getTime))))))

(defn calendar [date]
	(let
		[cal (new java.util.GregorianCalendar)]
		(. cal (setTime date))
		cal))

(defn diff-millis [d1 d2]
	(-
		(. (calendar d1) getTimeInMillis)
		(. (calendar d2) getTimeInMillis)))
(defn diff-seconds [d1 d2]
	(/ (diff-millis d1 d2) 1000))

(defn before
	"true if x is earlier than y"
	[x y]
	(neg?
		(compare x y)))


(defn schedule-task [timer f rate]
	(. timer
		(schedule
			(proxy [java.util.TimerTask] []
				(run [] (f)))
			(long 0)
			(long rate))))

(defn schedule-task-timer [f rate]
	(let [timer (new java.util.Timer)]
		(schedule-task timer f rate)
		timer))

