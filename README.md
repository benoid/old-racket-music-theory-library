# racket-music-theory - A Music Theory API for RSound.

Music Theory abstraction of the RSound library with support for notes, rests, measures and instruments.

Requires RSound, which is installable via raco.

# Demo
The demo will psuedo-randomly generate exerpts.
To run the demo generator, execute the following in a racket shell:

```
(require "demo.rkt")
(play-audio)

```

To stop the sound:

```
(stop-audio)
```
