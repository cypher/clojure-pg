(ns eu.philjordan.util.hash (:refer-clojure) (:require (eu.philjordan.util)))


(defn sha-256 [ba]
	(let [sha (. java.security.MessageDigest (getInstance "SHA-256"))]
		(. sha (digest ba))))

(defn sha-256-hex [ba]
	(eu.philjordan.util/bytearray-to-hex (sha-256 ba)))
	
(defn md5
	([ba]
		(let [sha (. java.security.MessageDigest (getInstance "MD5"))]
			(. sha (digest ba))))
	([str enc]
		(md5 (. str (getBytes enc)))))

(defn md5-hex 
	([ba]
		(eu.philjordan.util/bytearray-to-hex (md5 ba)))
	([str enc]
		(eu.philjordan.util/bytearray-to-hex (md5 str enc))))