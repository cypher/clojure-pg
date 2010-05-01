(ns at.metalab.clojure.pg.test
    ; (:require [eu.philjordan.util :as util])
    (:use [eu.philjordan.util :as util :only (prrn)])
    ; (:import eu.philjordan.util/prrn)
    )

(defstruct fsm-state
    :state          ;; Set with regex name and paths
    :transitions    ;; Map with all characters the machine can transition to from this state.
                    ;; The value is the index of the resulting state
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
    [token-specs meta-tokens]
    (struct fsm-state
        (reduce
            (fn
                [set [name & rule]]
                (apply conj set (flatten-paths name (enum-states (expand-meta-tokens rule meta-tokens) nil))))
            #{}
            token-specs)))

(defn token-rule-map
    [tokens meta-tokens]
    (reduce (fn [m [name & rule]] (assoc m name (expand-meta-tokens rule meta-tokens))) {} tokens))

(defn create-char-transition-map
    [states token-map]
    (reduce
        (fn
            [char-map [name path]]
            (let [rule (token-map name)]
                (if (= :eot path)
                    char-map
                    (update-in
                        char-map
                        [(in-rule rule path)]
                        #(apply util/set-conj %1 %2)
                        (flatten-paths name (mapcat #(enum-states rule %) (advance-path rule path)))))))
        {}
        states))

(defn register-state-from-transition
    [[state-vector state-map] state]
    (if (state-map state)
        [state-vector state-map]
        [(assoc state-vector (count state-vector) (struct-map fsm-state :state state :transitions {}))
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
                    assoc
                    char (state-map state)))
            state-vector
            transitions)))

(defn register-states
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
    (loop [state-vector [(initial-state tokens meta-tokens)]
           state-map {(:state (state-vector 0)) 0}
           idx 0]
        (if (>= idx (count state-vector))
            [state-vector state-map]
            (let [current-state (state-vector idx)
                  transition-map (create-char-transition-map (:state current-state) (token-rule-map tokens meta-tokens))
                  [state-vector state-map] (register-states current-state transition-map state-vector state-map)]
                (recur state-vector state-map (inc idx))))))

; java lexical grammar from
; http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html

(def *java-meta-tokens*
    (hash-map
        'LineTerminator '(| "\r" "\n" "\r\n" )
        'DecimalDigit (list* '| (map str (range 0 10)))))

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
            ;; '(EOLCOMMENT "//" (* (! LineTerminator)) LineTerminator)
            '(INTEGER (+ DecimalDigit))
            '(FLOAT
                (|
                    ((+ DecimalDigit) "." (* DecimalDigit))
                    ((* DecimalDigit) "." (+ DecimalDigit)))))))

; basic java grammar taken from
; http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html
(def *java-grammar*)