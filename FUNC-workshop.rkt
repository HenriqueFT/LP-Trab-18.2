#lang racket


(define executePV
  (lambda ()
    (void))
  )

(define executeU
  (lambda ()
    (void))
  )

(define execute*
  (lambda()
    (void))
  )

(define executeParentese
  (lambda(graf lista tail)
    (void))
  )

(define executeLetra
  (lambda(graf lista tail)
    (define label (first lista))
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
