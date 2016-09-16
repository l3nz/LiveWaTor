# LiveWaTor

A Wa-Tor implementation in Clojure for the browser, using figwheel and reagent.


References: 

* https://en.wikipedia.org/wiki/Wa-Tor



## Dev env

### Running

    lein figwheel

### Running tests

As running tests in JavaScript is complex and sucks, the model is defined as a 
.cljc class, so we can test int from  the JVM side of things.

	lein test

#### Tests in ClojureScript

To be understood - see https://github.com/emezeske/lein-cljsbuild/blob/master/doc/TESTING.md

    lein cljsbuild test

### Compile for production

    lein clean; lein cljsbuild once min

### Pretty-printing code

* http://pretty-print.net/ ma fai attenzione

