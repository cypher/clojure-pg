(ns clojure.pg.test
    (:require [eu.philjordan.util :as util])
  ;(:use )
  ;(:import )
    )


(defn ltok
    "generate a token spec for a literal token; the token will use the uppercased string for its symbol"
    [s]
    [(symbol (. s toUpperCase)) s]

; java lexical grammar from
; http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html

(def *java-meta-tokens*
    (vector
        ['LineTerminator '(| "\r" "\n" "\r\n" )]
        '[UnicodeInputCharacter
            #{ UnicodeEscape RawInputCharacter }]
        '[UnicodeEscape
            "\\" UnicodeMarker (4 HexDigit)]
        '[UnicodeMarker
            #{ "u" UnicodeMarker}]
        [RawInputCharacter
            ]

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
        []

; basic java grammar taken from
; http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html
(def *java-grammar*)