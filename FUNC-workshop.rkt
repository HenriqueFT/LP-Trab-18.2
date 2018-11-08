#lang racket


(define executePV
  (lambda ()
    (void))
  )

(define executeU
  (lambda (graf sub-lista tail)
    (void))
  )

(define execute*
  (lambda()
    (void))
  )

(define executeParentese
  (lambda(graf lista tail)
    (define extraido (extrair-ex lista)) ;lista : <sub-lista> , posicao-de-)
    (define temp-tail (list-tail lista (second extraido)))
    (define sub-lista (first extraido))
    (if (null? temp-tail)
        (FUNC graf sub-lista '());soh roda
        (cond
          [(symbol=? (first temp-tail) '|;|)
           (executePV)]
          [(symbol=? (first temp-tail) 'U)
           (define achado (encontra-PV temp-tail)) 
           (define next-list (first achado))
           (if (zero? (second achado))
               (executeU (list-tail temp-tail (+ 1 (second achado)(second extraido))));tail eeh o resto apos  (define tail (list-tail temp-tail (+ 1 (second achado)(second extraido))))
               (executeU '())); (define tail '())
           ]
          [(symbol=? (first temp-tail) '*)
           (execute*)]
          [else (if (FUNC graf (first extraido) '())
                    #t
                    #f)]
          )
        )
    )
  )

(define executeLetra
  (lambda(graf lista tail)
    (define label (first lista))
    (if (null? (cdr lista))
        (FUNC graf label tail)
        (cond
          [(symbol=? (second lista) '|;|)(executePV)]
          [(symbol=? (second lista) 'U)(executeU)]
          [(symbol=? (second lista) '*)(execute*)]
          [else (if (or-map label graf tail FUNC)
                    #t
                    #f)]
          )
        )
    )
  )

(define FUNC
  (lambda (graf lista tail)
    (if (null? (car lista)) ;caso FUNC tenha a  instrucao sendo uma lista '() ,quer dizer que chegou no final,entao reetorna #t
        #t
        (if (symbol=? (first lista) '|(| )
            (executeParentese(graf lista tail));aqui executaremos tudo caso tenha ( na frente
            (executeLetra(graf lista tail))
          )
        )
    )
  )

;-----------------------------------------------------------------Coisas apos aquija estao implementadas----------------------------------------

(define or-map
  (lambda ()
    (void))
  )

(define extrair-ex
  (lambda ()
    (void))
  )

(define encontra-PV
  (lambda()
    (void))
  )