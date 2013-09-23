(module utils racket
  (provide string-wrap)
  
  (define (string-wrap str width)
    (string-join (map (λ (line)
                        (add-words-wrap "" (string-split line " ") width))
                      (string-split str "\n"))
                 "\n"))
  
  (define (add-words-wrap body words width)
    (if (empty? words)
        body
        (add-words-wrap
         (add-word-wrap body (first words) width)
         (rest words)
         width)))
  
  (define (add-word-wrap body word width)
    (if (<= (+ (last-line-length body)
               (string-length word))
            width)
        (string-append body word " ")
        (string-append body "\n" word " ")))
  
  ; Returns the length of the text following the last \n in str.
  (define (last-line-length str)
    (if (string=? "" str)
        0
        (string-length (last (string-split (string-trim str) "\n"))))))
