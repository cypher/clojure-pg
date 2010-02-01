`clojure-pg`
============

`clojure-pg` is a parser generator written in Clojure.

It is very much a work in progress and lacks documentation in any form.

Usage
-----

* Open a Clojure REPL in the src directory. Make sure the CLASSPATH includes the current working directory (i.e. "`.`")
* Load the `clojure.pg.test` namespace: `(require 'clojure.pg.test)`
* Change into the `clojure.pg.test` namespace: `(ns clojure.pg.test)`
* Generate a state machine for the lexer based on a small subset of Java tokens: `(generate-state-machine *java-tokens* *java-meta-tokens*)`

License
-------

`clojure-pg` is dual-licensed under the BSD License (see file [COPYING](COPYING)) and the Eclipse Public License v1.0 (see file [epl-v10.html](epl-v10.html)).
