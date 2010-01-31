(ns clojure.pg.test
    ; (:require [eu.philjordan.util :as util])
    (:use [eu.philjordan.util :as util :only (prrn)])
    ; (:import eu.philjordan.util/prrn)
    )


(defn ltok
    "generate a token spec for a literal token; the token will use the uppercased string for its symbol"
    [s]
    [(symbol (. s toUpperCase)) s])

(defn in-regex
    "obtains the sub-rule at the specified location in the given regular expression. Locations are specified as a vector of indices into the tree."
    [[name & rules] path]
    (loop [rule rules
           path path]
           ;; (prn path rule)
           (if (not-empty path)
               (recur (nth rule (first path)) (rest path))
               rule)))


(defn- advance-path
    [regex path]
    (let [prefix (drop-last path)
          tail-idx (last path)
          tail (util/append prefix (inc tail-idx))
          op (in-regex regex (util/append prefix 0))]
        (cond
            (= '| op)
                (recur regex prefix)
            (empty? (in-regex regex tail))
                (if (#{'+ '*} op) 
                    (util/append (advance-path regex prefix) (util/append prefix 1))
                    (recur regex prefix))
            :else
                tail
            )))

(defn- enum-or-states
    [path rules]
    (map #(util/append path %) (range 1 (inc (count rules)))))

(defn- enum-*-states
    [regex path]
    (list (util/append path 1) (advance-path regex path)))

(defn- enum-+-states
    [path]
    (list (util/append path 1)))

(def enum-?-states #'enum-*-states)

(defn epsilon-transitions1
    "Determines the possible states reachable by one epsilon transition from the current state."
    [regex path]
    (let [rule (in-regex regex path)]
        (cond
            (string? rule)
                ; literal
                nil
            (symbol? rule)
                ; TODO: expand meta-token
                nil
            (seq? rule)
                (let [[op & rest] rule]
                    (condp = op
                        '| (enum-or-states path rest)
                        '* (enum-*-states regex path)
                        '+ (enum-+-states path)
                        ;; '! '() ;; not yet implemented/used
                        '? (enum-?-states regex path)
                        (list (util/append path 0)) ))
            :else
                (list (util/append path 0)) )))

(defn enum-states
    "Enumerates all possible states after applying the maximum number of epsilon transitions from the specified state."
    [regex path]
    (util/prrn (if-let [expansions (epsilon-transitions1 regex path)]
        (mapcat #(enum-states regex %) expansions)
        (list path))))

(defn transition-literals
    ""
    [regex path]
    (map #(in-regex regex %) (enum-states regex path)))

(def expand-meta-tokens)

(defn- expand-meta-token
    [rule meta-tokens]
    (cond
        (symbol? rule)
            (or (meta-tokens rule) rule)
        (list? rule)
            (expand-meta-tokens rule meta-tokens)
        :else
            rule))

(defn expand-meta-tokens
    [rule meta-tokens]
    (map #(expand-meta-token % meta-tokens) rule))

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
