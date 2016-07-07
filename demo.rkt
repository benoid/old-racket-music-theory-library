#lang racket

;; Driver file for game

(require "./audio/synthesis-framework.rkt")
(require "./audio/tonal-harmony.rkt")
(require "./audio/audio-generator.rkt")
(provide (all-from-out "./audio/synthesis-framework.rkt"
                       "./audio/audio-generator.rkt"
                       "./audio/tonal-harmony.rkt"))

(display "Use the function play-audio to generate an audio sample")
(newline)

