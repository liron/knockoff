2009-08-10

thanks to pg and rtm for arc and rhickey and all the #clojure people for clojure.

the initial goal was to get the arc challenge running.  i ended up doing a bit 
more, but this is still just a clone of (most of) srv.arc, it doesn't have a lot of 
arc's html stuff yet. but it probably will soon.

also let me know if there's anything wrong with the license or copyright or whatever, 
i never thought about that stuff until this morning.

also github made me pick a name, so i went with "knockoff", but call it whatever you want.
besides it should be j-encoded like a real clojure software project: "kjnocjkofjf"

to run it:

First open up srv.clj and change parent* to the checkout directory.

Also get clojure (from github, perhaps)
and get clojure-contrib (also on github)

put those things and this directory on your classpath; maybe by firing up java like:

java -cp "/path/to/clojure.jar:/path/to/clojure-contrib.jar:/path/to/this/directory" clojure.main

then at the repl:
(load "srv")
(def srv* (thread (serve)))

serve takes an optional port number.

now you can point your browser to localhost:8080 or localhost:8080/said

check out the bottom of srv.clj for the definition of said
also there's arc examples out there you can copy.

to quit without killing the repl:
(reset! quitsrv* true)

after a couple more requests the thread will die.
