

### racket-music-theory - A Music Theory Abstraction for Racket's RSound library.

Note: This project is being rewritten and incorporated into a fork of rsound [here](https://github.com/benoid/RSound/tree/master/rsound/composer).

Music Theory abstraction of the RSound library with support for notes, rests, measures and instruments.

Racket is available at https://download.racket-lang.org/

This library requires RSound, which is installable via ```raco```.

```raco pkg install rsound```

# To use this library
This project is not packaged as a racket library, it is just a collection of source files.  ```require``` whichever files you want to use.  

# Demo
The demo will psuedo-randomly generate exerpts.
To run the demo generator, execute the following in a ```racket``` shell in the root directory of the git project:

```
(require "demo.rkt")
(play-audio)

```

To stop the sound:

```
(stop-audio)
```
