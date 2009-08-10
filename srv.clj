; released 2009-08-10
(load "util" "html")
(comment
  "Copyright: (c) 2009 Liron Greenstein
  This program is free software. It comes without any warranty, to
  the extent permitted by applicable law. You can redistribute it
  and/or modify it under the terms of the Do What The Fuck You Want
  To Public License, Version 2, as published by Sam Hocevar. See
  COPYING.txt for more details.")

(declare ensure-srvdirs handle-request handle-request-1 abusive-ip 
handle-request-thread log-request handle-post static-filetype 
respond respond-err parseheader parseurl parseargs parsecookies 
harvest-fnids srvlog )

(defs 
  parent* "/Users/liron/clj"
  cljdir* (str parent* "/clj/")
  logdir* (str parent* "/clj/logs/")
  staticdir* (str parent* "/static/")
  quitsrv* (atom nil)
  breaksrv* nil
  currsock* (atom nil))

(defn serve
  ([] (serve 8080)) ; can only recur to same arity...
  ([port]
     (wipe quitsrv*)
     (ensure-srvdirs)
     (w-socket s port
     ; FIXME: implement setuid (no mortbay jars)
       (prln "ready to serve port " port)
       (flush)
       (reset! currsock* s)
       (until @quitsrv* ; FIXME: trouble reopening socket after quitting
	 (handle-request s breaksrv*)))
     (prln "quit server")))

(defn serve1
  ([] (serve1 8080))
  ([port] (w-socket s port (handle-request s true))))

(defn ensure-srvdirs []
  (dorun (map ensure-dir (list cljdir* logdir* staticdir*))))

(def srv-noisy* (atom nil))

(defs threadlife* 30
      requests* (atom 0)
      requests-ip* (atom {})
      throttle-ips* (atom {})
      ignore-ips* (atom {})
      spurned* (atom {}))

(defn handle-request [s breaksrv]
  (if breaksrv
    (handle-request-1 s)
    (errsafe (handle-request-1 s))))

(defn handle-request-1 [s]
  (let [[i o ip] (socket-accept s)]
    (if (and (or (@ignore-ips* ip) (abusive-ip ip))
	     (++ spurned* {:key ip :default 0})) ; FIXME: yeah, i know
      (close i o)
      (do (++ requests*)
	  (++ requests-ip* {:key ip :default 0})
	  (let [th1 (atom nil) th2 (atom nil)]
	    (reset! th1 (thread
			  (after (handle-request-thread i o ip)
				 (close i o)
				 (break-thread @th2)))) ; FIXME: kill-thread is break-thread.. change that?
	    (reset! th2 (thread
			  (sleep threadlife*)
			  (unless (dead @th1)
				  (prln "srv thread took too long for " ip))
			  (break-thread @th1)
			  (close i o))))))))

(defs req-times* (atom {}) req-limit* 30 req-window* 10 dos-window* 2)

(defn abusive-ip [ip]
  (and ((only >) (@requests-ip* ip) 250)
       (let [now (sec)]
	 (do1 (if (@req-times* ip)
		(and (>= (qlen (@req-times* ip))
			 (if (@throttle-ips* ip) 1 req-limit*))
		     (let [dt (- now (deq (@req-times* ip)))]
		       (if (< dt dos-window*) (hset (ignore-ips* ip)))
		       (< dt req-window*)))
		(do (hset (req-times* ip) (queue))
		    nil))
	      (enq now (@req-times* ip))))))

(defn handle-request-thread [i o ip]
  (let [nls (atom 0) lines (atom nil) line (atom nil) responded (atom nil) t0 (msec)]
    (after
     (do (whilet c (unless @responded (readc i))
	   (if @srv-noisy* (prl c))
	   (if (is c \newline)
	     (if (is (++ nls) 2)
	       (let [[rtype op args n cooks] (parseheader (rev @lines))
		     t1 (msec)]
		 (if @srv-noisy* (println))
		 (case rtype
		       'get (respond o op args cooks ip)
		       'post (handle-post i o op args n cooks ip)
		       (respond-err o "Unknown request: " (car @lines)))
		 (log-request rtype op args cooks ip t0 t1)
		 (reset! responded true))
	       (do (push (apply str (rev @line)) lines)
		   (wipe line)))
	     (unless (is c \return)
		     (push c line)
		     (reset! nls 0)))))
     (close i o)))
  (harvest-fnids))

(defn log-request [rtype op args cooks ip t0 t1]
  (let [parsetime (- t1 t0) respondtime (- (msec) t1)]
    (srvlog 'srv
	    ip
	    parsetime
	    respondtime
	    (if (> (+ parsetime respondtime) 1000) "***" "")
	    rtype
	    op
	    (let [arg1 (car args)]
	      (if (caris arg1 "fnid") ""  arg1))
	    cooks)))


(defn handle-post [i o op args n cooks ip]
  (if @srv-noisy* (prl "Post Contents: "))
  (if (no n)
    (respond-err o "Post request without Content-Length.")
    (let [line (atom nil) ln (atom n)]
      (whilet c (and (> @ln 0) (readc i))
	(if @srv-noisy* (prl c))
	(-- ln)
	(push c line))
      (if @srv-noisy* (prln "\n")) ; why did paul pr \n\n..?
      (respond o op (into (parseargs (apply str (rev @line))) args) cooks ip))))

(def header* "HTTP/1.1 200 OK
Content-type: text/html; charset=utf-8
Connection: close")

(def type-header* (atom {}))

(defn gen-type-header [ctype]
  (str "HTTP/1.0 200 OK
Content-type: "
       ctype
       "
Connection: close"))

(doall (map (fn [[k v]] (hset (type-header* k) (gen-type-header v)))
	    '((gif          "image/gif")
	      (jpg          "image/jpeg")
	      (png          "image/png")
	      (text/html    "text/html; charset=utf-8"))))

(def rdheader* "HTTP/1.0 302 Moved")

(defs srvops* (atom {}) redirector* (atom {}) optimes* (atom {}) opcounts* (atom {}))

(defn save-optime [name elapsed]
  (++ opcounts* {:default 0 :key name})
  (unless (@optimes* name) (hset (optimes* name) (queue)))
  (enq-limit elapsed (@optimes* name) 1000))

(defmacro defop-raw [name parms & body]
  `(hset (srvops* '~name)
	 (fn ~(doall (vec parms))
	   (let [t1# (msec)]
	     (do1 (do ~@body)
		  (save-optime '~name (- (msec) t1#)))))))

(defmacro defopr-raw [name parms & body]
  `(do (hset (redirector* '~name) true)
       (hset (srvops* '~name) (fn ~(vec parms) ~@body))))

(defmacro defop [name parm & body]
  `(do (swap! redirector* dissoc '~name)
       (defop-raw ~name (gs# ~parm)
	 (w-stdout gs# (prln) ~@body))))

(defmacro defopr [name parm & body]
  `(do (hset (redirector* '~name))
       (defop-raw ~name (gs# ~parm)
	 ~@body)))

(deftem request
  args nil
  cooks nil
  ip nil)

(defs unknown-msg* "Unknown." max-age* (atom {}) static-max-age* (atom nil))

(defn respond [strn op args cooks ip]
;  (let [strn *out*]
  (w-stdout strn
    (iflet f (@srvops* op)
	   (let [req (inst 'request 'args args 'cooks cooks 'ip ip)]
	     (if (@redirector* op)
	       (do (prln rdheader*)
		   (prln "Location: " (f strn req))
		   (prln))
	       (do (prln header*)
		   (awhen (@max-age* op)
		     (prln "Cache-Control: max-age=" it))
		   (f strn req))))
	   (let [filetype (static-filetype op)]
	     (aif (and filetype (file-exists (str staticdir* op)))
	       (do (prln (@type-header* filetype))
		   (awhen @static-max-age*
		     (prln "Cache-Control: max-age=" it))
		   (prln)
		   (w-infile i it
		     (whilet b (readb i)
		       (writeb b strn))))
	       (respond-err strn unknown-msg*))))))
;					)

(defn static-filetype [symb]
  (let [fname (str symb)]
    (and (not (re-find #"/" fname))
	 (case (downcase (last (check (tokens fname #"\.") #(not (single %)))))
	       "gif"  'gif
	       "jpg"  'jpg
	       "jpeg" 'jpg
	       "png"  'png
	       "ico"  'png
	       "css"  'text/html
	       "txt"  'text/html
	       "htm"  'text/html
	       "html" 'text/html
	       "clj"  'text/html
	       "arc"  'text/html ;D
	       nil))))


(defn respond-err [strn msg & args]
  (w-stdout strn
    (prln header*)
    (prln)
    (apply prl msg args)))

(defn parseheader [lines]
  (let [[type op args] (parseurl (car lines))]
    (list type
	  op
	  args
	  (and (= type 'post)
	       (some (fn [s]
		       (and (re-find #"^Content-Length:" s)
			    (errsafe (mkint (cadr (tokens s))))))
		     (cdr lines)))
	  (some (fn [s]
		  (and (re-find #"^Cookie:" s)
		       (parsecookies s)))
		(cdr lines)))))

(defn parseurl [s]
  (let [[type url] (tokens s)
	[base args] (tokens url #"\?")]
    (list (sym (downcase type))
	  (let [op (cut base 1)]
	    (if (empty? op)
	      (sym "||")
	      (sym op)))
	  (if args
	    (parseargs args)
	    nil))))

(defn parseargs [s]
  (seq (map (fn [[k v]] (list k (urldecode v))) ; urldecode
	      (map #(tokens % #"=") (tokens s #"&")))))

(defn parsecookies [s]
  (doall (map #(tokens % #"=")
	      (cdr (tokens s #"[\s\;]+")))))

(defn arg [req key]
  (alref (req 'args) key))

(defs fns* (ref {}) fnids* (ref nil) timed-fnids* (ref nil))

(defn new-fnid []
  (check (sym (rand-string 10)) #(not (@fns* %)) (recur)))

(defn fnid [f]
  (dosync
   (let [key (new-fnid)]
     (alter fns* assoc key f)
     (alter fnids* conj key)
     key)))

(defn timed-fnid [lasts f]
  (dosync
   (let [key (new-fnid)]
     (alter fns* assoc key f)
     (alter timed-fnids* conj (list key (sec) lasts))
     key)))

(defmacro afnid [f]
  `(dosync
    (let [~'it (new-fnid)]
      (alter fns* assoc ~'it ~f)
      (alter fnids* conj ~'it)
      ~'it)))

(defn harvest-fnids
  ([] (harvest-fnids 50000))
  ([n] (when (len> @fns* n)
	 (dosync
	  (ref-set timed-fnids*
		   (pull (fn [[id created lasts]]
			   (when (> (since created) lasts)
			     (alter fns* dissoc id)
			     true))
			 @timed-fnids*))
	  (let [nharvest (quot n 10)]
	    (let [[kill keep] (split (rev @fnids*) nharvest)]
	      (ref-set fnids* (rev keep))
	      (dorun (for [id kill]
		       (alter fns* dissoc id)))))))))

(defs fnurl* "/x" rfnurl* "/r" rfnurl2* "/y" jfnurl* "/a")

(def dead-msg* "\nUnknown or expired link.")

(defop-raw x (strn req)
  (w-stdout strn
    (aif (@fns* (sym (arg req "fnid")))
      (it req)
      (prl dead-msg*))))

(defopr-raw y (strn req)
  (aif (@fns* (sym (arg req "fnid")))
    (w-stdout str (it req))
    "deadlink"))

(defop-raw a (strn req)
  (aif (@fns* (sym (arg req "fnid")))
       (tostring (it req))))

(defopr r req
  (aif (@fns* (sym (arg req "fnid")))
    (it req)
    "deadlink"))

(defop deadlink req
  (pr dead-msg*)) ; recent arc source has some messed up variable but news.yc seems fixed

(defn url-for [fnid]
  (str fnurl* "?fnid=" fnid))

(defn flink [f]
  (str fnurl* "?fnid=" (fnid (fn [req] (prln) (f req)))))

(defn rflink [f]
  (str rfnurl* "?fnid=" (fnid f)))

(defmacro w-link [expr & body]
  `(tag (~'a ~'href (flink (fn [~(gensym)] ~expr))) ; tag and a
     ~@body))

(defmacro w-rlink [expr & body]
  `(tag (~'a ~'href (rflink (fn [~(gensym)] ~expr)))
     ~@body))

(defmacro onlink [text & body]
  `(w-link (do ~@body) (prl ~text)))

(defmacro onrlink [text & body]
  `(w-rlink (do ~@body) (prl ~text)))

(defmacro linkf [text parms & body]
  `(tag (a href (flink (fn ~(vec parms) ~@body))) (prl ~text)))

(defmacro rlinkf [text parms & body]
  `(tag (~'a ~'href (rflink (fn ~(vec parms) ~@body))) (prl ~text)))

(defmacro w-link-if [test expr & body]
  `(tag-if ~test (~'a ~'href (flink (fn [~(gensym)] ~expr)))
     ~@body))

(defn fnid-field [id]
  (gentag input type 'hidden name 'fnid value id))

(defn fnform 
  ([f bodyfn] (fnform f bodyfn nil))
  ([f bodyfn redir] (tag (form method 'post action (if redir rfnurl2* fnurl*))
		      (fnid-field (fnid f))
		      (bodyfn))))

(defmacro aform [f & body]
  `(tag (~'form ~'method 'post ~'action fnurl*)
     (fnid-field (fnid (fn [ga#]
			 (prln)
			 (~f ga#))))
     ~@body))

(defmacro taform [lasts f & body]
  `(let [gl# ~lasts
	 gf# (fn [ga#] (prln) (~f ga#))]
     (tag (~'form ~'method 'post ~'action fnurl*)
       (fnid-field (if gl# (timed-fnid gl# gf#) (fnid gf#)))
       ~@body)))

(defmacro arform [f & body]
  `(tag (~'form ~'method 'post ~'action rfnurl*)
     (fnid-field (fnid ~f))
     ~@body))

(defmacro tarform [lasts f & body]
  `(let [gl# ~lasts gf# ~f]
     (tag (~'form ~'method 'post ~'action rfnurl*)
       (fnid-field (if gl# (timed-fnid gl# gf#) (fnid gf#)))
       ~@body)))

(defmacro aformh [f & body]
  `(tag (~'form ~'method 'post ~'action fnurl*)
     (fnid-field (fnid ~f))
     ~@body))

(defmacro arformh [f & body]
  `(tag (~'form ~'method 'post ~'action rfnurl2*)
     (fnid-field (fnid ~f))
     ~@body))

(def unique-ids* (atom {}))

(defn unique-id
  ([] (unique-id 8))
  ([size] `(let [id (sum (rand-string (max size 5)))]
	     (if (@unique-ids* id)
	       (recur)
	       (hset (unique-ids* id) id)))))

(let [lastasked (ref nil) lastval (ref nil)]
  
  (defn memodate []
    (let [now (sec)]
      (if (or (no @lastasked) (> (- now @lastasked) 60))
	(dosync
	 (ref-set lastasked now)
	 (ref-set lastval (datestring)))
	@lastval)))

  )

(defn logfile-name [type]
  (str logdir* type "-" (memodate)))

(mkobj lock
  (defn srvlog [type & args]
    (w-appendfile o (logfile-name type)
      (w-stdout o (locking lock
		    (apply prs (sec) (map strict args)) (prln))))))

(defop || req (prl "It's alive.")) ; yeah the || thing is a hack

(defop said req
  (aform #(onlink "click here" (prl "you said: " (arg % "foo")))
    (input "foo")
    (submit)))

; bg-threads?  how many clojure wheels can i reinvent...