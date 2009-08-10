; released 2009-08-10
(comment
  "Copyright: (c) 2009 Liron Greenstein
  This program is free software. It comes without any warranty, to
  the extent permitted by applicable law. You can redistribute it
  and/or modify it under the terms of the Do What The Fuck You Want
  To Public License, Version 2, as published by Sam Hocevar. See
  COPYING.txt for more details.")

(defn pr-escaped [x]
  (dorun (for [c x]
	   (prl (case c 
		      \< "&#60;"
		      \> "&#62;"
		      \" "&#34;"
		      \& "&#38;"
		      c)))))

(def opmeths* (atom {}))

(defmacro opmeth [& args]
  `(@opmeths* (list ~@args)))

(defn opstring [key val]
  `(aif ~val (prl ~(str " " key "=\"") ~'it \")))

(defn opsym [key val]
  `(prl ~(str " " key "=") ~val))

(defn opnum [key val]
  `(aif ~val (prl ~(str " " key "=") ~'it)))

(defn opcheck [key val]
  `(if ~val (prl " checked")))

(defn opesc [key val]
  `(awhen ~val
     (prl ~(str " " key "=\""))
     (if (string? ~'it) (pr-escaped ~'it) (prl ~'it))
     (prl \")))

(defmacro attribute [tag opt f]
  `(hset (opmeths* (list '~tag '~opt)) ~f))

(attribute a          href           opstring)
(attribute a          rel            opstring)
(attribute a          class          opstring)
(attribute a          id             opsym)
(attribute a          onclick        opstring)
(attribute input      name           opstring)
(attribute input      size           opnum)
(attribute input      type           opsym)
(attribute input      value          opesc)
(attribute input      checked        opcheck)
(attribute form       action         opsym)
(attribute form       method         opstring)

(defn literal [x]
  (cond (symbol? x) false
	(coll? x) (caris x 'quote)
	true, true)) ; i couldn't resist

(comment
  I'm guessing this was for atstrings...here for completeness
(defn precomputable-tagopt [val]
  (and (literal val)            
       (no (and (string? val) (re-find #"@" val))))))

(defn tag-options [spec options]
  (if (no options)
    nil
    (let [[[opt val] & rst] options]
      (let [meth (if (= opt 'style) opstring (opmeth spec opt))]
	    (if meth
	      (if val
		(cons (if (literal val)
			(tostring (eval (meth opt val)))
			(meth opt val))
		      (tag-options spec rst))
		(tag-options spec rst))
	      (do
		(prl "<!-- ignoring " opt " for " spec "-->")
		(tag-options spec rst)))))))

(defn start-tag [spec]
  (if (lone spec)
    `(prl ~(str "<" spec ">"))
    (let [opts (tag-options (car spec) (pair (cdr spec)))]
      (if (every? string? opts)
	`(prl ~(str "<" (car spec) (apply str opts) ">"))
	`(do (prl ~(str "<" (car spec)))
	     ~@(seq (map (fn [opt]
		      (if (string? opt)
			`(prl ~opt)
			opt))
		    opts))
	     (prl ">"))))))

(defn end-tag [spec]
  `(prl ~(str "</" (carif spec) ">")))

(defmacro tag [spec & body]
  `(do ~(start-tag spec)
       ~@body
       ~(end-tag spec)))

(defmacro tag-if [test spec & body]
  `(if ~test
     (tag ~spec ~@body)
     (do ~@body)))

(defmacro gentag [& args] (start-tag args))

(defn input
  ([name] (input name "" 10))
  ([name val] (input name val 10))
  ([name val size] (gentag input type 'text name name value val size size)))

(defn submit
  ([] (submit "submit"))
  ([val] (gentag input type 'submit value val)))