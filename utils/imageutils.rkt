(module imageutils racket
  (require 2htdp/image)
  (require "utils.rkt")
  (provide paragraph)
  
  ; Takes a newline delimited string, and draws it split into lines.
  (define (paragraph lines size color
                     #:align     [align "left"]
                     #:wrap      [wrap #f]
                     #:font      [font #f]
                     #:family    [family 'system]
                     #:style     [style 'normal]
                     #:weight    [weight 'normal]
                     #:underline [underline #f]) 
    (let* ([str-lines (char-escapes
                       (if (string? lines)
                           lines
                           (string-join lines "\n")))]
           [lines (string-split
                   (if (boolean? wrap)
                       str-lines
                       (string-wrap
                        str-lines
                        wrap))
                   "\n")])
      (cond [(empty? lines) empty-image]
            [else (above/align
                   align
                   (if (boolean? font)
                       (text (first lines) size color)
                       (text/font (first lines) size color
                                  font family style weight underline))
                   (paragraph (rest lines) size color
                              #:align     align
                              #:font      font
                              #:family    family
                              #:style     style
                              #:weight    weight
                              #:underline underline))])))
  
  (define (char-escapes s)
    (string-replace
     (string-replace
      (string-replace
       (string-replace
        s "%%" "%")
       "%n" "\n")
      "%r" "\r")
     "%t" "\t")))