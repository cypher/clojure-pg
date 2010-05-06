(ns at.metalab.clojure.pg.test
    (:require [eu.philjordan.util.rangemap :as rangemap])
    (:use [eu.philjordan.util :as util :only (prrn)])
    ; (:import eu.philjordan.util/prrn)
    )

(defstruct fsm-state
    :state          ;; Set with regex name and paths
    :transitions    ;; Map with all characters the machine can transition to from this state.
                    ;; The value is the index of the resulting state
    )

(defstruct token
	:type
	:str
	)

(defn ltok
    "generate a token spec for a literal token; the token will use the uppercased string for its symbol"
    [s]
    [(symbol (. s toUpperCase)) s])

(defn in-rule
	""
	[rule path]
	(if (not-empty path)
		(recur (nth rule (first path)) (rest path))
		rule))

(defn rest-rule
    ""
    [rule path]
    (drop (last path) (in-rule rule (drop-last path))))

(defn in-regex
    "obtains the sub-rule at the specified location in the given regular expression. Locations are specified as a vector of indices into the tree."
    [[name & rules] path]
    (in-rule rules path))

(defn- advance-path
    [rule path]
    (let [prefix (drop-last path)
          tail-idx (last path)
          tail (util/append prefix (inc tail-idx))
          op (in-rule rule (util/append prefix 0))]
        (cond
            (= '| op)
                (recur rule prefix)
            (empty? (rest-rule rule tail))
                (cond
                    (#{'+ '*} op)
                        (util/append (advance-path rule prefix) (util/append prefix 1))
                    (empty? prefix)
                        (list :eot)
                    :else
                        (recur rule prefix))
            :else
                (list tail)
            )))

(defn- enum-or-states
    [path rules]
    (map #(util/append path %) (range 1 (inc (count rules)))))

(defn- enum-*-states
    [rule path]
    (list* (util/append path 1) (advance-path rule path)))

(defn- enum-+-states
    [path]
    (list (util/append path 1)))

(def enum-?-states #'enum-*-states)

(defn epsilon-transitions1
	"Determines the possible states reachable by one epsilon transition from the current state."
	[start-rule path]
	(let [rule (in-rule start-rule path)]
		(cond
			(char? rule)
				; literal
				nil
			(symbol? rule)
				; TODO: expand meta-token
				nil
			(and (vector? rule) (keyword? (first rule)))
				; some higher-level named rule
				(let [rulename (first rule)]
					(condp = rulename
						; don't touch character ranges, they are processed in create-char-transition-map
						:range nil
						; don't do anything to unknown rules, create-char-transition-map will generate a warning.
						nil))
			(seq? rule)
				(let [[op & rest] rule]
					(condp = op
						'| (enum-or-states path rest)
						'* (enum-*-states start-rule path)
						'+ (enum-+-states path)
						;; '! '() ;; not yet implemented/used
						'? (enum-?-states start-rule path)
						(list (util/append path 0)) ))
			:else
				(list (util/append path 0)) )))

(defn enum-states
    "Enumerates all possible states after applying the maximum number of epsilon transitions from the specified state."
    [rule path]
    (if (= path :eot)
        (list path)
        (if-let [expansions (epsilon-transitions1 rule path)]
            (mapcat #(enum-states rule %) expansions)
            (list path))))

(defn transition-literals
    ""
    [rule path]
    (map #(in-rule rule %) (enum-states rule path)))

(def expand-meta-tokens)

(defn- expand-meta-token
    [rule meta-tokens]
    (cond
        (symbol? rule)
            (or
                (expand-meta-token (meta-tokens rule) meta-tokens)
                rule)
        (seq? rule)
            (expand-meta-tokens rule meta-tokens)
        (string? rule)
            ;; No need to use a list for one-character rules
            (if (= (count rule) 1)
                (first rule)
                (seq rule))
        :else
            rule))

(defn expand-meta-tokens
    [rule meta-tokens]
    (map #(#'expand-meta-token % meta-tokens) rule))

(defn- flatten-paths
    [name paths]
    (map #(list name %) paths))

(defn initial-state
	"Generates the fsm-state before any characters have been consumed, where all
	 rules are in null positions, and no transitions have been calculated."
	[token-specs meta-tokens]
	(struct fsm-state
		(reduce
			(fn
				[set [name & rule]]
				(apply conj set (flatten-paths name (enum-states (expand-meta-tokens rule meta-tokens) nil))))
			#{}
			token-specs)
		(rangemap/range-map)))

(defn token-rule-map
    [tokens meta-tokens]
    (reduce (fn [m [name & rule]] (assoc m name (expand-meta-tokens rule meta-tokens))) {} tokens))

(defn range-rule? [rule]
	(and (vector? rule) (= :range (first rule))))

(defn inversion-rule? [rule]
	(and (vector? rule) (= :not (first rule))))

(defn insert-range-dest-states
	"Insert the given range into the character range map, taking care of any overlap."
	[cr-map range add-states]
	;(prn 'insert-range-dest-states cr-map range add-states)
	(apply
		rangemap/update-range cr-map range
		util/set-conj add-states))

(defn create-char-transition-map
	"Given the set of rule states, produces a map of character transitions to other states."
	[states token-map]
	(reduce
		(fn
			[char-map [name path]]
			(let [rule (token-map name)]
				(if
					(= :eot path)
						char-map
					(let [term (in-rule rule path)]
						(cond
							(char? term)
								; add the post-transition state to the character entry in the transition table, creating an entry if necessary
								(insert-range-dest-states
									char-map
									(rangemap/char-range term)
									(flatten-paths name (mapcat #(enum-states rule %) (advance-path rule path))))
							(range-rule? term)
								(insert-range-dest-states
									char-map
									(apply rangemap/char-range (rest term)) ; convert inclusive range to one usable by range-map
									(flatten-paths name (mapcat #(enum-states rule %) (advance-path rule path))))
							(inversion-rule? term)
								; The [:not ...] rule contains a sequence of ranges or literals, which must be inverted
								(let
									; first, convert everything to [start, end) ranges and sort them (]
									[ranges
										(apply rangemap/range-set (map
											#(if (char? %) (rangemap/char-range %) (apply rangemap/char-range %))
											(rest term)))
									 inverted-ranges
										(rangemap/range-gaps
											[(char java.lang.Character/MIN_VALUE) (char java.lang.Character/MAX_VALUE)]
											ranges)
									 next-paths (flatten-paths name (mapcat #(enum-states rule %) (advance-path rule path)))]
									;(prn inverted-ranges)
									(reduce
										#(insert-range-dest-states %1 %2 next-paths)
										char-map
										inverted-ranges))
							:else
								(do
									(println "Unknown terminal type: " term " - ignoring.")
									char-map))))))
		(rangemap/range-map)
		states))

(defn register-state-from-transition
	"Checks if the given state already exists, and if not, appends it to the state
   vector, and enters it into the state map."
	[[state-vector state-map] state]
	(if (state-map state)
		[state-vector state-map]
		[(assoc state-vector (count state-vector) (struct-map fsm-state :state state :transitions (rangemap/range-map)))
		 (assoc state-map state (count state-vector))]))

(defn register-state-transitions
    [from-state transitions state-vector state-map]
    (let [from-state-idx (state-map (:state from-state))]
        (reduce
            (fn
                [v [char state]]
                (update-in
                    v
                    [from-state-idx :transitions]
                    (fn [m k val] (when-not m (prn "warn:" from-state-idx v)) (assoc m k val))
                    char (state-map state)))
            state-vector
            transitions)))

(defn register-states
	"Adds any newly created states to the state vector & map, adds the
   discovered transition-map to the state object"
	[from-state transition-map state-vector state-map]
	(let
		[states (vals transition-map)
		 [state-vector state-map]
			(reduce
				register-state-from-transition
				[state-vector state-map]
				states)
		 state-vector (register-state-transitions from-state transition-map state-vector state-map)]
		[state-vector state-map]))

(defn generate-state-machine
	[tokens meta-tokens]
	"Given the specified token and meta-token grammars, produces the entire state table"
	(loop [state-vector [(initial-state tokens meta-tokens)] ; build the initial state object
				 state-map {(:state (state-vector 0)) 0} ; initial state has index 0, allow indexing by rule state
				 idx 0] ; current state to inspect
		(if (>= idx (count state-vector))
			[state-vector state-map] ; the number of inspected states has caught up with the produced states. -> no remaining reachable states.
			(let
				[current-state (state-vector idx)
				 ; inspect the current state for allowed characters, and which states they lead to
				 transition-map (create-char-transition-map (:state current-state) (token-rule-map tokens meta-tokens))
				 ; expand the set of available states if additional ones were discovered in the previous step
				 [state-vector state-map] (register-states current-state transition-map state-vector state-map)]
				(recur state-vector state-map (inc idx))))))

(defn completed-tokens
	[rule-states consumed remain]
	; pick out all token types which are in end-of-token state
	(let
		[end-states (filter #(= :eot (second %)) rule-states)
	   s (when end-states (apply str (reverse consumed)))]
		(map #(hash-map :type (first %) :name s  :remain remain)
			end-states)))

(defn compare-tokens-by-len
	[{a :name  at :type } {b :name  bt :type}]
	(let [cmp (- (compare (count a) (count b)))]
		(if-not (zero? cmp) cmp
			(compare at bt))))

(defn produce-possible-tokens
	"Generates a set of possible tokens that could be produced by consuming the first characters of char-stream"
	[state-vector char-stream]
	(loop
		[state-idx 0
		 consumed-chars nil
		 chars (seq char-stream)
	   ; sort by length, descending
		 produced-tokens (sorted-set-by compare-tokens-by-len)]
		(if-not chars
			produced-tokens
			(let
				[ch (first chars)
				 { trans :transitions } (state-vector state-idx)
				 to-idx (get trans ch)
				 to-rule-states (when to-idx (:state (state-vector to-idx)))]
				(if
					(or
						(zero? (count trans)) ; dead end, no more transitions
						(not to-idx)) ; dead end, this char isn't a valid state transition
					produced-tokens
					; move to new state
					(let
						[consumed (cons ch consumed-chars)
						 remain (rest chars)
						 completed (completed-tokens to-rule-states consumed remain)
						 tokens
							(if (empty? completed)
								produced-tokens
								(apply conj produced-tokens completed))]
						(recur to-idx consumed remain tokens)))))))

(defn produce-token
	"Produces a token, if possible, by consuming a many characters of char-stream
   as possible using the state-vector and breaking ties using the
   token-transitions table."
	[state-vector token-transitions char-stream]
	(let [possible-tokens (produce-possible-tokens state-vector char-stream)]
		(when-not (empty? possible-tokens)
			(let
				[ref-tok (first possible-tokens)
		     len (count (:name ref-tok))
				 ; list of all tokens with the same length as the first
				 tie-toks (filter #(= len (count (:name %))) (rest possible-tokens))
				 ; pick out token with fewest transitions
		     {type :type  name :name  rem :remain}
				  (if tie-toks
						(apply min-key
								#(token-transitions (:type %))
								ref-tok tie-toks)
						ref-tok)]
				[(struct token type name) rem]))))

(defn read-tokens
	[state-vector token-transitions char-stream]
	(when-not (empty? char-stream)
		(lazy-seq
			(let [[tok rem] (produce-token state-vector token-transitions char-stream)]
				(if tok
					(cons tok (read-tokens state-vector token-transitions rem))
					(list (struct token 'TOKENIZATION-ERROR (apply str char-stream))))))))


(defn token-transitions-table
	[state-vector]
	(let
		; count incoming transitions into each FSM state
		[incoming-transitions-by-state
			(reduce
				(fn [v {t :transitions}]
					; for each transition, count eligible chars and credit them to destination
					(reduce
						(fn [v [[start end] dest]]
							(update-in v [dest] + (- (int end) (int start))))
						v
						t))
				; vector with incoming transition count for each state. Always start at state 0, so it starts with 1 incoming.
				(assoc (vec (repeat (count state-vector) 0)) 0 1)
				state-vector)]
		; for each rule state in an FSM state, multiply the transitions for that rule by that FSM state's incoming transitions
		(reduce
			; iterate through FSM states
			(fn [m [{rule-states :state} num-transitions]]
				(reduce
					(fn [m [rule]]
						(assoc m rule
							(*
								(get m rule 1) ; transitions for that rule so far, defaulting to 1
								num-transitions)))
					m
					rule-states))
			{}
			(util/cluster-seq 2 (interleave state-vector incoming-transitions-by-state)))))

(defn lexer [token-rules meta-token-rules]
	(let
		[[state-vector] (generate-state-machine token-rules meta-token-rules)
	   transitions (token-transitions-table state-vector)]
		(fn generated-lexer [chars]
			(read-tokens state-vector transitions chars))))


; java lexical structure from
; http://java.sun.com/docs/books/jls/third_edition/html/lexical.html

(def *java-meta-tokens*
	(hash-map
		'LineTerminator '(| "\r" "\n" "\r\n" )
		'DecimalDigit [:range \0 \9]
		'Alpha '(| [:range \A \Z] [:range \a \z])
		'Alnum '(| Alpha DecimalDigit)))

(def *java-tokens*
	(concat
		(map ltok
			(list
				; logic/bitwise
				"||" "&&" "|" "^" "&"
				; comparison
				"==" "!=" "<" ">" "<=" ">="
				; arithmetic
				"<<" ">>" ">>>" "+" "-" "*" "/" "%"
				; assignment
				"=" "+=" "-=" "*=" "/=" "&=" "|=" "^=" "%=" "<<=" ">>=" ">>>="))
		(list
			'(EOLCOMMENT "//" (* [:not \return \newline]) LineTerminator)
			'(INTEGER (+ DecimalDigit))
			'(FLOAT
				(|
					((+ DecimalDigit) "." (* DecimalDigit))
					((* DecimalDigit) "." (+ DecimalDigit))))
			'(IDENTIFIER
				(Alpha (* Alnum)))
			'(KEYWORD
				(| "public" "private" "static")))))

; basic java grammar taken from
; http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html
(def *java-grammar*)
