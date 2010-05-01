
(ns eu.philjordan.util.rangemap
  ;(:require )
  ;(:use )
  ;(:import )
  )

; range-based map. A sorted-map where keys are simple [start end] vectors, where
; start is inclusive and end is exclusive. All starts and ends in a range-map
; must be comparable in all combinations.
; Since the range-map is not a multimap, ranges must not overlap. Attempting to
; assoc an overlapping range results in a java.lang.ArithmeticException being
; thrown.

(defn range-map-range-compare
	[[a-start a-end] [b-start b-end]]
	(cond
		(and (= a-end b-end) (= a-start b-start))
			0
		(<= (compare a-end b-start) 0) ; a before b
			-1
		(<= (compare b-end a-start) 0) ; b before a
			1
		:else
			(throw (new java.lang.ArithmeticException "range-map ranges must not overlap unless they are exactly equal"))))

(defn range-map-key-compare
	[[start end] key]
	(cond
		(<= (compare end key) 0)
			-1
		(< (compare key start) 0)
			1
		:else
			0))

(defn range-map-compare [a b]
	(try
		(let [as (seq a)]
			(try
				(let [bs (seq b)]
					(range-map-range-compare as bs))
				(catch java.lang.IllegalArgumentException e
					; b is not seqable, treat it as key
					(range-map-key-compare as b))))
		(catch java.lang.IllegalArgumentException e
			; a is not seqable, treat it as key
			(- (range-map-key-compare b a)))))

; everything below: INCOMPLETE

; range-based multimap. Keys are specified as ranges, Lookup of a key will
; return a set of all values for which the key falls in the corresponding range.
;
; Underlying data structure is a sorted-map, indexed by range start. Overlapping
; ranges are split.
; Like so:
;
;	{
;		{ :start 0  :end 2  :mappings  #{
;			{ :start 0  :end 5  :value :foo }
;			{ :start 0  :end 7  :value :bar }}}
;		{ :start 2  :end 5  :mappings  #{
;			{ :start 0  :end 5  :value :foo }
;			{ :start 0  :end 7  :value :bar }
;			{ :start 2  :end 7  :value :baz }}}
;		{ :start 5  :end 7  :mappings  #{
;			{ :start 0  :end 7  :value :bar }
;			{ :start 2  :end 7  :value :baz }}}}


(comment

(defstruct range-mapping
	:start
	:end
	:value)

(defstruct range-mmap-entry
	:start
	:end
	:mappings)

(defn sort-range-mmap-entry [{a :start} {b :start}]
	(compare a b))


(defn get-ranges
	"Look up key in rmap, which is a range-multimap, and returns a set of all range mappings into which key falls"
	[rmap key]
	(when-let [lbound (first (subseq rmap >= key))]
		(when (neg? (compare key (:end lbound)))
			(:mappings lbound))))

(defn get-range-vals
	"Look up key in rmap, which is a range-multimap, and returns a seq of all values into whose ranges key falls"
	[rmap key]
	(map :value (get-ranges rmap key)))

(defn range-mmap-entry-gaps [[{ st :start  :as f } & overlaps] start end fill-with]
	(if-not f
		; no existing ranges, it's all one big gap
		(list (struct range-mmap-entry start end fill-with))
		; fill in any gap before the first range, then iterate
		(loop
			[gaps (when (> st start) (list (struct range-mmap-entry start st fill-with)))
			 { pend :end  :as p } f
			 more overlaps]
			(let [{ nstart :start  :as next } (first more)]
				(if next
					(recur
						(if (> nstart pend) ; insert a range-mmap-entry if there's a gap
							(conj gaps (struct range-mmap-entry pend nstart fill-with))
							gaps)
						next
						(rest more))
					; p is the last one, insert a gap if it ends before our range
					(if (< pend end)
						(conj gaps (struct range-mmap-entry pend end fill-with))
						gaps))))))

(defn assoc-range-mapping
	[rmap { s :start  e :end  :as range} ]
	(let
		[overlaps (subseq rmap >= s < e)
		 rmap
			(reduce
				(fn [rmap { rs :start  re :end  :as range }]
					; TODO: implement
					))]))

(defn assoc-range
	([rmap from to val]
		(assoc-range-mapping rmap (struct range-mapping from to val)))
	([rmap from to val & kvs]
		(let [ret (assoc rmap from to val)]
			(if kvs
				(recur ret (first kvs) (second kvs) (nnext kvs))
					ret))))

(defn range-multimap
	([& keyvals]
		(let [rmap (sorted-map-by sort-range-mmap-entry)]
			(if keyvals
				(apply assoc-range rmap keyvals)
				rmap))))
)