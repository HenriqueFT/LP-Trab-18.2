#lang racket


(define executePV
  (lambda (graf sub-lista tail)
    (if(FUNC graf sub-lista tail)
       (FUNC graf tail '())
       (#f)))
  )

(define executeU
  (lambda (tail)
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
           (executePV graf sub-lista tail)]
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

(define not-void?;umaprocedure que faz  o oposto de void? , feita na necessidade de filtrarmos voids
  (lambda (any)
    (if (void? any)
        #f
        #t)
    )
  )

(define or-map
  (lambda ()
    (void))
  )

(define extrair-ex
  (lambda ()
    (void))
  )

(define encontra-PV-void
  (lambda (sym-list)
    
    (let ([counter 0]
          [achado #f]
          [buffer '()])
      (map (lambda (s-atual)
             (if achado
                 (void)
                 (cond
                   [(symbol=? s-atual '|(| )
                    (if (positive? counter) ;em ambos os casos (ser >0 ou nao) iremos incrementar
                        (begin
                          (set! buffer (append buffer (list s-atual)))
                          (set! counter (+ counter 1)))
                        (set! counter (+ counter 1)))
                    ]
                   [(symbol=? s-atual '|)| )
                    (set! counter (- counter 1))
                    (if (positive? counter) ;se 0 iremos fazer a  funcao, e nunca vai ser 0 sem ter tirado um ( antes, a nao  ser que apenas nao tenha
                        (set! buffer (append buffer (list s-atual)))
                        buffer)]
                   [(and (symbol=? s-atual '|;| ) (zero? counter))
                    (set! achado #t )
                    (set! buffer (append buffer #t))
                    buffer
                    
                 
                    ]
                   [else (set! buffer (append buffer (list s-atual)))]
                   )
                 
                 )
             )
           (list-tail sym-list 1))
      )

    
    )
  )

(define encontra-PV
  (lambda (sym-list)
   (define retorno(filter not-void? (encontra-PV-void sym-list)))
    (if (boolean? (last retorno))
        (begin
          (remove #t retorno)
          (list retorno (+ (length retorno) 2 ))
          
          )
        (list retorno 0)
    )))
      