#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "utils/imageutils.rkt")
(require "utils/textengine.rkt")

(define (draw-game g)
  (overlay/align/offset
   "center" "bottom"
   (draw-options (send g location-options)
                 (send g selected))
   0 30
   (overlay/align/offset
    "center" "top"
    (paragraph (send g location-text) 15 "white"
               #:wrap 65)
    0 -30
    (rectangle 500 500 "solid" "black"))))

(define (draw-options o selected)
  (if (> (length o) 0)
      (beside (draw-option (first o)
                           (zero? selected))
              (if (> (length o) 1)
                  (rectangle 50 0 "solid" "black")
                  empty-image)
              (draw-options (rest o) (sub1 selected)))
      empty-image))

(define (draw-option o selected?)
  (paragraph o 15 (if selected?
                      "white"
                      "darkgray")))

(define (key-pressed g key)
  (cond [(key=? key "left")
         (send g select-next!)]
        [(key=? key "right")
         (send g select-prev!)]
        [(or (key=? key " ")
             (key=? key "\r")
             (key=? key "up"))
         (send g do-selected!)]
        [else (void)]) g)

(big-bang (make-object game% "pits.yaml")
          (to-draw draw-game)
          (on-key key-pressed))