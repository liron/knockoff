; released 2009-08-10
(comment
  "Copyright: (c) 2009 Liron Greenstein
  This program is free software. It comes without any warranty, to
  the extent permitted by applicable law. You can redistribute it
  and/or modify it under the terms of the Do What The Fuck You Want
  To Public License, Version 2, as published by Sam Hocevar. See
  COPYING.txt for more details.")

(import [java.io File InputStreamReader StringWriter StringReader PushbackReader
	 InputStream FileInputStream FileOutputStream]
	[java.util TimeZone Calendar]
	'java.lang.InterruptedException
	'java.beans.Introspector
	'java.net.ServerSocket)
(use '[clojure.contrib.str-utils :only (re-split)])

; basic stuff
(defmacro is [& body]
  `(= ~@body))

(defn sym
  ([name] (symbol name))
  ([ns name] (symbol ns name)))

(defn no [arg]
  (or (not arg) (if (coll? arg) (empty? arg))))

; sequences
(defn car [lst]   ; hahaha
  (first lst))

(defn cdr [lst]
  (rest lst))

(defn caar [lst]
  (car (car lst)))

(defn cadr [lst]
  (car (cdr lst)))

(defn caddr [lst]
  (car (cdr (cdr lst))))

(defn cddr [lst]
  (cdr (cdr lst)))

(defn single [x]
  (and (coll? x) (no (cdr x))))

(defn lone [x] (not (coll? x))) ; atom was taken

(defn len [sq] (count sq))

(defn strict [x] ; lazy by default is a bad idea
  (if (coll? x) (seq x) x))

(defn rev [lst]
  (reverse lst))

(defn mappend [f & args]
  (rev (reduce into nil (apply map f args))))

(defn pair
  ([lst] (pair lst list))
  ([lst f]
     (loop [lst lst f f acc nil]
       (if (no lst)
	 (rev acc)
	 (if (< (len lst) 2)
	   (list (list (car lst)))
	   (recur (cddr lst) f (cons (f (car lst) (cadr lst)) acc)))))))

(defn cut
  ([sequ start] (cut sequ start (len sequ)))
  ([sequ start end] (let [sequ2 (drop start (take end sequ))]
		      (if (= (class sequ) String)
			(apply str sequ2)
			sequ2))))

(defn split [sequ pos]
  (list (cut sequ 0 pos) (cut sequ pos)))

(defmacro in [x & choices]
  (let [g (gensym)]
    `(let [~g ~x]
       (or ~@(map (fn [c] `(= ~g ~c)) choices)))))

(defn lookup [key al]
  (if (no (coll? al))
    nil
    (if (and (coll? (car al)) (= (caar al) key))
      (car al)
      (recur key (cdr al)))))

(defn alref [al key]
  (cadr (lookup key al)))

(defn pull [pred coll]
  (seq (filter (complement pred) coll)))

; more-general functions
(defn len> [x n]
  (> (len x) n))

(defn caris [x val]
  (and (coll? x) (is (car x) val)))

(defmacro check
  ([x test] `(check ~x ~test nil))
  ([x test alt] `(let [gx# ~x]
		   (if (~test gx#) 
		     gx# 
		     ~alt))))

(defn only [f]
  (fn [& args] (if (car args) (apply f args))))

; control structures
(defmacro do1 [& args]
  `(let [g# ~(car args)]
     ~@(cdr args)
     g#))

(defmacro until [test & body]
  `(while (not ~test) ~@body))

(defmacro whilet [var test & body]
  `(letfn [(gf# [gp#]
		(let [~var gp#]
		  (when ~var ~@body (recur ~test))))]
     (gf# ~test)))

(defmacro forlen [i s & body]
  `(dorun (for [~i (range 0 (len ~s))] ~@body)))

(defmacro case [& body]
  `(condp = ~@body))

(defmacro unless [& body]
  `(when-not ~@body)) ; when-not is dumb

(defmacro aif [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       ~(car body)
       (do ~@(cdr body))))) ; not perfect...

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (when ~'it ~@body)))

(defmacro caselet [v expr & args]
  (letfn [(ex [args]
	      (if (no (cdr args))
		(car args)
		`(if (= ~v '~(car args))
		   ~(cadr args)
		   ~(ex (cddr args)))))]
    `(let [~v ~expr] ~(ex args))))

(defn carif [x] (if (coll? x) (car x) x))

(defmacro iflet [var expr then & rest]
  `(let [gv# ~expr]
     (if gv# 
       (let [~var gv#] ~then) 
       ~@rest)))

(defmacro whenlet [var expr & body]
  `(iflet ~var ~expr (do ~@body)))

; general error handling

(defn protect [during after]
  (try (during)
       (finally (after))))

(defmacro after [x & ys]
  `(protect (fn [] ~x) (fn [] ~@ys)))

(defmacro errsafe [expr] ; well it's err-mostly-safe
  `(try ~expr
	(catch Exception gs#
	  (if (instance? InterruptedException gs#)
	    (throw (InterruptedException "thread interrupted."))
	    nil))))

(defmacro err [& body]
  `(throw (Exception. (str ~@body))))

; ops on atoms
(defn push [obj sequ]
  (swap! sequ conj obj))

(defmacro hset 
  ([hshref] `(hset ~hshref true))
  ([hshref val] (let [[hsh key] hshref] `(swap! ~hsh assoc ~key ~val))))

(defn incdec
  ([updateFn num] (swap! num updateFn 1))
  ([updateFn hsh {default :default amt :amt key :key}]
     (if key (swap! hsh assoc key 
		    (updateFn (or (@hsh key) default) (or amt 1)))
	 (reset! hsh (updateFn (or @hsh default) (or amt 1))))))

(defn ++ 
  ([num] (incdec + num))
  ([num hsh] (incdec + num hsh)))

(defn --
  ([num] (incdec - num))
  ([num hsh] (incdec - num hsh)))

(defn wipe [& args]
  (dorun
   (map #(reset! % nil)
	args)))

; date/time
(defn msec []
  (System/currentTimeMillis))

(defn sec [] ; seconds conflicted with rosado's p5 lib
  (quot (msec) 1000))

(defn since [t1] (- (sec) t1))

(defn date
  ([] (date (sec)))
  ([s] (let [ms (* s 1000)
	     cal (. Calendar getInstance (. TimeZone getTimeZone "GMT"))]
	 (. cal setTimeInMillis ms)
	 [(.get cal Calendar/YEAR) (inc (.get cal Calendar/MONTH)) (.get cal Calendar/DATE)])))

(defn datestring
  ([] (datestring (sec)))
  ([s] (let [[y m d] (date s)]
	 (str y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d))))

; string ops

(defn downcase [strn]
  (case (class strn)
	java.lang.String    (. strn toLowerCase)
	java.lang.Character (Character/toLowerCase strn)
	nil                 '||   ; hack (ha as if the rest of this isn't)
	clojure.lang.Symbol (sym (.toLowerCase (str strn)))
	(err "Can't downcase " strn)))

(defmacro tokens
  ([strn] `(tokens ~strn #"\s"))
  ([strn rx] `(re-split ~rx ~strn)))

(defn rand-string [n]
  (let [c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (loop [s nil i 0]
      (if (>= i n)
	(apply str s)
	(recur (conj s (nth c (rand-int (len c)))) (inc i))))))

; files and i/o

(defn dir-exists [path]
  (let [f (File. path)]
    (and (.isDirectory f) f)))

(defn file-exists [path]
  (let [f (File. path)]
    (and (.isFile f) f)))

(defn ensure-dir [path]
  (unless (dir-exists path) (.mkdirs (File. path))))

(defmacro w-stdout [s & body]
  `(binding [*out* ~s]
     ~@body))

(defmacro w-stdin [s & body]
  `(binding [*in* ~s]
     ~@body))

(defmacro w-stderr [s & body]
  `(binding [*err* ~s]
     ~@body))

(defn writeb
  ([b] (writeb b *out*))
  ([b o] (errsafe (. o write (int b)))))

(defn writec [& body] ; hmm..
  (apply writeb body))

(defn readb
  ([] (readb *in*))
  ([i] (errsafe
	(let [rv (.read i)]
	  (if (< rv 0) nil (byte rv))))))

(let [readtbl (atom {})] ; memory leak by interrupted threads..?

; if you ever readc from something, you can't then readb from it
; that's the common case anyway
  (defn readc
    ([] (readc *in*))
    ([i] 
       (let [rv (if (instance? InputStream i)
		  (do (if (no (@readtbl i))
			(hset (readtbl i) (InputStreamReader. i)))
		      (let [c (.read (@readtbl i))]
			(if (< c 0) (swap! readtbl dissoc i))
			c))
		  (.read i))]
	 (if (< rv 0) nil (char rv)))))
  )

(defn socket-accept [ss]
  (let [sock (.accept ss)]
    (list (.getInputStream sock)
	  (.getOutputStream sock)
	  (.getHostAddress (.getInetAddress sock)))))

(defn instring [str]
  (PushbackReader. (StringReader. str)))

(defn outstring []
  (StringWriter.))

(defn inside [os]
  (. os toString))

(defn infile [path]
  (FileInputStream. path))

(defn outfile [path]
  (FileOutputStream. path))

(defn appendfile [path]
  (FileOutputStream. path true))

(defmacro w-socket [s port & body]
  `(let [~s (ServerSocket. ~port)]
     (do ~@body
	 (.close ~s))))

(defn close [& body]
  (dorun (for [elt body]
	   (.close elt))))

(letfn [(expander [f var name body]
		  `(let [~var (~f ~name)]
		     (after (do ~@body) (close ~var))))]

  (defmacro w-instring [var str & body]
    (expander 'instring var str body))

  (defmacro w-infile [var str & body]
    (expander 'infile var str body))

  (defmacro w-outfile [var str & body]
    (expander 'outfile var str body))
  
  (defmacro w-appendfile [var str & body]
    (expander 'appendfile var str body))

  )
(defmacro w-outstring [var & body]
  `(let [~var (outstring)] ~@body))

(defn prl [& body]
  (let [s (apply str body)
	chars (vec (.toCharArray s))]
    (dorun (map #(writec %) chars)))
  (car body))

(defn prln [& body]
  (do1 (apply prl body)
       (writec \newline)))

(defn prs [& body]
  (doall (map prl (interpose \space body)))
  body)

(defmacro tostring [& body]
  `(w-outstring gv#
     (w-stdout gv# ~@body)
     (inside gv#)))

(defmacro erp [& body]
  `(w-stdout *err*
     (println "err: " ~@body)))

(comment ; if you're on old clojure w/no transients, use an atom for strn
(defn readstr []
  (let [strn (transient [])]
    (whilet c (readc)
      (conj! strn c))
    (apply str (persistent! strn)))))

; threads
(defmacro thread [& body]
  `(let [gt# (Thread. (fn []
			(try (do ~@body)
			     (catch InterruptedException ge# nil))))]
     (.start gt#)
     gt#))

(defn dead [th]
  (or (not (.isAlive th)) nil))

(defn break-thread [th]
  (and th (.interrupt th))
  th)

; queues
(defn queue []
  (atom [ [] 0 (Object.)]))

(defn qlen [q]
  (len (@q 0)))

(defn  enq [obj q]
  (let [lock (caddr @q)]
    (locking lock
      (let [[cont size _] @q]
	(reset! q [(conj cont obj) (inc size) lock])
	(@q 0)))))

(defn deq [q]
  (let [lock (caddr @q)]
    (locking lock
      (let [[cont size _] @q]
	(if (> size 0)
	  (let [[f & r] cont]
	    (reset! q [(vec r) (dec size) lock])
	    f)
	  cont)))))

(defn enq-limit
  ([val q] (enq-limit val q 1000))
  ([val q limit]
     (let [lock (caddr @q)]
       (locking lock
	 (unless (< (qlen q) limit)
		 (deq q))
	 (enq val q)))))

; templates (the bare minimum!)
(def templates* (atom {}))

(defmacro deftem [tem & fields]
  (let [name (carif tem) 
	includes (if (coll? tem) (cdr tem))]
    `(swap! templates* assoc '~name 
	    (into (mappend @templates* '~(rev includes))
		  (list ~@(map (fn [[k v]] `(list '~k (fn [] ~v)))
			       (pair fields)))))))

(defn inst [tem & args]
  (let [x (atom {})] ; transient hashes would be nice
    (dorun (for [[k v] (@templates* tem)]
	     (unless (no v) (swap! x assoc k (v)))))
    (dorun (for [[k v] (pair args)]
	     (swap! x assoc k v)))
    @x))


; java ops

(defn gc [] (.. Runtime getRuntime gc))

(defn cmeths 
  ([obj] (cmeths obj #(.getMethod %)))
  ([obj meth] (dorun (for [m (map meth (.getMethodDescriptors (Introspector/getBeanInfo obj)))]
		       (println m)))))

; print methods of an instance
(defn meths [obj]
  (cmeths (class obj) #(.getMethod %)))

; just the names of methods of an instance
(defn meth-names [obj]
  (cmeths (class obj) #(.getName %)))

; just the names of methods of a class
(defn cmeth-names [obj]
  (cmeths obj #(.getName %)))

; os
(defmacro popen [cmd i o e & body]
  `(let [proc# (.. Runtime getRuntime (exec (str ~cmd)))
	 ~i (.getInputStream proc#)
	 ~o (.getOutputStream proc#)
	 ~e (.getErrorStream proc#)
	 rv# (do ~@body)]
     (.waitFor proc#)
     (close ~i ~o ~e)
     rv#))

(defn system [cmd]
  (popen cmd i o e
    (w-stdin i (whilet c (readc i) (writec c)))
    nil))

(comment ; i bet mzscheme has something like popen that arc could use
  (defn shash [strn]
    (let [res (popen "openssl dgst -sha1" i o e
		(w-stdout o (prl strn))
		(close o)
		(w-stdin i (readstr)))]
      (cut res 0 (dec (len res))))))

; sort of random

(defmacro defs [& body]
  (let [tups (pair body)
	toset (map (fn [[k v]]
		     `(def ~k ~v))
		   tups)]
    `(do ~@toset)))

(defn sleep [ms]
  (. Thread sleep (* ms 1000)))

(defmacro mac [name parms & body] ; ha...nah
    `(defmacro ~name ~(vec parms)
       ~@body))

(defn mkint
  ([obj] (Integer. obj))
  ([obj radix] (. Integer parseInt obj radix)))

(defmacro mkobj [obj & body]
  `(let [~obj (Object.)] ~@body))

(defn urldecode [s]
  (tostring
    (loop [i 0]
      (if (< i (len s))
	(caselet c (get s i)
		 \+ (do
		      (writec \space)
		      (recur (inc i)))
		 \% (do (when (> (- (len s) i) 2)
			  (writeb (mkint (cut s (+ i 1) (+ i 3)) 16)))
			(recur (+ i 3)))
		 (do (writec c)
		     (recur (inc i))))))))
    
(comment ;doesn't work, no objects in clojure's macroexpansions...
(defmacro atomic [ & body] ; ...nor mzscheme's, says pg
  (let [o (Object.)]
    `(locking o ~@body))))
