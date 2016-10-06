# racket-music-theory - A Music Theory Abstraction for Racket's RSound library.
Music Theory abstraction of the RSound library with support for notes, rests, measures and instruments.

Racket is available at https://download.racket-lang.org/

This library requires RSound, which is installable via ```raco```.

```raco pkg install rsound```



# Demo
The demo will psuedo-randomly generate exerpts.
To run the demo generator, execute the following in a ```racket``` shell:

```
(require "demo.rkt")
(play-audio)

```

To stop the sound:

```
(stop-audio)
```
