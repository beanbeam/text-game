(module textengine racket
  (require (planet esilkensen/yaml:2:=1))
  (provide game%)
  
  (define game%
    (class object%
      (init definition-file)
      (super-make-object)
      (define game-defs (read-yaml (open-input-file definition-file)))
      (define current-loc (start))
      (define selected-option 0)
      
      (define/private (start)
        (if (hash-has-key? game-defs "start")
            (hash-ref game-defs "start")
            "start"))
      
      (define/private (location)
        (hash-ref (hash-ref game-defs
                                      "locations")
                            current-loc))
      
      (define/public (location-text)
        (hash-ref (location) "text"))
      
      (define/private (raw-options)
        (hash-ref (location) "options"))
      
      (define/public (location-options)
        (map (λ (o)
               (hash-ref o "text"))
             (raw-options)))        
      
      (define/public (do! o)
        (set! current-loc
              (hash-ref (list-ref (raw-options) o)
                        "goto"))
        (set! selected-option 0))
      
      (define/public (do-selected!)
        (do! (selected)))
      
      (define/public (select! o)
        (set! selected-option o))
      
      (define/public (selected)
        selected-option)
      
      (define/public (select-next!)
        (select! (modulo (add1 (selected)) (length (raw-options)))))
      
      (define/public (select-prev!)
        (select! (modulo (add1 (selected)) (length (raw-options))))))))