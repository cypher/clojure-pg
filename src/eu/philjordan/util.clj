(ns eu.philjordan.util (:refer-clojure))

(defn assoc-pushvec
	"pushes v onto the end of the vector associated with k in map, or associates [v] with k if not already in map."
	[map k v]
	(assoc map k
		(if (contains? map k)
			(let [cv (map k)] ; current val
				(if (vector? cv)
					(assoc cv (count cv) v) ; assoc as new last element
					[cv v]))
			v)))

(defn xml-to-simple-tree
	"convert output from clojure.xml/parse to a simple hash tree with vectors if more than one instance of a tag is found at a level. Nil is the key for text elements if there are other elements at that level"
	[xml]
	(cond
		(string? xml)
			xml
		(vector? xml)
			(if (and (= 1 (count xml)) (string? (xml 0)))
				(xml 0)
				(reduce
					(fn [tags cur]
						(assoc-pushvec tags
							(:tag cur)
							(xml-to-simple-tree (or (:content cur) cur))))
					{}
					xml))
		(map? xml)
			{ (:tag xml) (xml-to-simple-tree (:content xml)) }
		:else
			xml))

(defn cons-if
	"if (pred x) is truthy, cons it onto seq, else just return seq. pred is identity in 2-param variant."
	([x seq]
		(if x
			(cons x seq)
			seq))
	([x pred seq]
		(if (pred x)
			(cons x seq)
			seq)))

(defn xml-to-compact
	"convert output from clojure.xml/parse to the compact format used by clojure.contrib.prxml"
	[{tag :tag  attrs :attrs  content :content  :as item}]
	(if (or (string? item) (not item))
		item
		(vec
			(cons tag
				(cons-if attrs #(not (empty? %))
					(map xml-to-compact
						content))))))
;		{:tag :poker, :attrs nil, :content [{:tag :invite, :attrs nil, :content [{:tag :user, :attrs {:id "614291362"}, :content nil}]}]})

(defn xml-first-tag
	[tag xml]
	(some
		(fn [[t & data]]
			(and (= tag t) data))
		xml))

(defn xml-get-attr [attr d]
	(when d
		(let [[a] d]
			(when (map? a)
				(get a attr)))))

(defn random-uuid []
	(. java.util.UUID (randomUUID)))

(defn swap-prev!
	"like swap! but returns the previous value of the atom"
	[atom f & args]
	(loop []
		(let [oldval @atom newval (apply f oldval args)]
			(if (compare-and-set! atom oldval newval)
				oldval
				(recur)))))

(defn string-inputstream [s]
	(new java.io.ByteArrayInputStream (. s (getBytes "UTF-8"))))
	
(defn combination-vecs
	"generates a sequence of 2-vectors for each combination of the elements of s1 and s2"
	[s1 s2]
	(mapcat
		(fn [e1]
			(map
				(fn [e2]
					[e1 e2])
				s2))
		s1))

(defn map-invert [map]
	"reverses the key-value mapping in map, i.e. { 1 'A  2 'B } becomes { 'A 1  'B }. Duplicates are silently dropped in no particular order."
	(reduce
		(fn [m [k v]]
			(assoc m v k))
		{}
		map))
(defn vec-invert [v]
	"inverts the index->value mapping of the vector to a value->index hashmap. [\"a\" \"b\" \"c\"] becomes {\"c\" 2, \"b\" 1, \"a\" 0}"
	(map-invert
		(map #(vector %2 %1)
			v
			(iterate inc 0))))

(defn map-invert-multi [map]
	(reduce
		(fn [m [k v]]
			(assoc m v (conj (m v) k)))
		{}
		map))

(defn pairs
	"returns a list of 2-vectors containing all pairs of neighbours in seq. '(1 2 3 4 5) -> ([1 2] [2 3] [3 4] [4 5])"
	[seq]
	(map
		vector
		seq
		(rest seq)))

(defn every-pair?
	"Returns true if pred is logical true for every pair of neighbours in seq."
	[pred seq]
	(every? (fn [[a b]](pred a b)) (pairs seq)))

(defn cluster-seq
	"Split the seq s into a lazy seq of lists with length clen."
	[clen s]
	(when-not (empty? s)
		(lazy-seq
			(let [[cl rem] (split-at clen s)]
				(cons
					cl
					(cluster-seq clen rem))))))
				

(defonce *rng* (new java.security.SecureRandom))
(defn random-int [ubound]
	(. *rng* (nextInt ubound)))

(defn vector-remove-nopreserve
	"remove element idx from vector v, move the last element to fill the gap."
	[v idx]
	(let [vcp (if (= idx (dec (count v))) v (assoc v idx (last v)))]
		(pop vcp)))

(defn choose-random-items
	"Randomly choose n items from coll with no duplicates."
	[coll n]
	(first
		(reduce
			(fn [[dest src] i]
				(let [idx (random-int (count src))]
					[(cons (src idx) dest)
					 (vector-remove-nopreserve src idx)]))
			[() (vec coll)] (range n))))

(def #^{:doc "Like map, but produces a vector."}
	mapvec (comp vec map))

(defn max-by
	"Find the largest element in coll using cmp for comparing elements."
	[cmp coll]
	(reduce
		#(if (neg? (cmp %1 %2)) %2 %1)
		coll))

(defn str-to-int
	"Converts a string to an integer value. The second argument, if present, is returned if the string does not represent an integer."
	([s]
		(new java.math.BigInteger s))
	([s onerr]
		(try (str-to-int s)
			(catch NumberFormatException e onerr))))

(defn repeat-str
	"Concatenate num repetitions of rep-sc."
	[rep-sc num]
	(apply str (map (constantly rep-sc) (range 0 num))))

(defn bytearray-to-hex
	"Returns a string of hex digits representing the given byte array. The string will always contain 2 digits for every byte, including any necessary leading zeroes."
	[byte-array]
	(let
		[hex (. (new BigInteger 1 byte-array) (toString 16))
		 delta-len (- (* 2 (count byte-array)) (count hex))]
		(if (= 0 delta-len)
			hex
			(str (repeat-str "0" delta-len) hex))))

(defn assoc-when
	"In the given map, associate each key with its corresponding val, but only if the value is truthy."
	[map & kvs]
	(reduce
		(fn [m [k v]]
			(if v
				(assoc m k v)
				m))
		map
		(cluster-seq 2 kvs)))

(defn filter* "like filter, but accepts extra arguments to the predicate"
	[pred coll & args]
	(filter #(apply pred % args) coll))

(defn prrn "Like prn, but accepts only one argument and returns it after printing." [x]
	(prn x)
	x)
	

(defn str-starts-with?
	"Determines whether the string s starts with prefix."
	[s prefix]
	(. s (startsWith prefix)))
