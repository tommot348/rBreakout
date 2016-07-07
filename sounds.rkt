#lang racket/base
(require 3s)
(define brick-hit-sound (path->audio (string-append (path->string (current-directory)) "audio/brick.wav")))
(define wall-hit-sound (path->audio (string-append (path->string (current-directory)) "audio/wall.wav")))
(define select-sound (path->audio (string-append (path->string (current-directory)) "audio/select.wav")))
(define bg-sound (path->audio (string-append (path->string (current-directory)) "audio/bg.wav")))
;(string-append (path->string (current-directory)) "audio/brick.wav")
;(audio? brick-hit-sound)

(define context (make-sound-context))
;(sound-context? context)

(define system-state (initial-system-state context))
;(system-state? system-state)

(define bhs (sound-at brick-hit-sound #:gain 0.1 1.0+1.0i))
(define whs (sound-at wall-hit-sound #:gain 64.0 1.0+1.0i))
(define selso (sound-at select-sound #:gain 64.0 1.0+1.0i))
;(define background-track (sound-at bg-sound 50.0+50.0i #:gain 0.9 #:looping? #t))
(define background-track (background (lambda (a) bg-sound)))
;(sound-pause! system-state)
(define (play-bg) (render-sound system-state 0.1 5.0+5.0i (exact->inexact 0.0) (list background-track)))
(define (play-sounds sounds) (render-sound system-state 0.1 5.0+5.0i (exact->inexact 0.0) sounds))

(define (stop-playback) (sound-context-destroy! context))

(provide play-sounds play-bg stop-playback selso whs bhs)