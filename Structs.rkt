#lang racket

(provide (all-defined-out))

;Define the figures as structure
(struct piece
    (name color position rank image selected-image)
  )

;States
(struct worldstate (pieces selected turncolor))
