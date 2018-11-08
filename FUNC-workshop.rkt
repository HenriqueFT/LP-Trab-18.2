#lang racket


(define executePV
  (lambda (graf lista tail)
    (define lista-sem-comando (list-tail lista 1))  
    (if(FUNC graf  lista-sem-comando tail)
       (FUNC graf tail '())
       #f))
  )

(define executeU
  (lambda (graf lista tail)
    (define lista-sem-comando (list-tail lista 1))  
    (define lista-de-programas (encontra-U tail))
    (if (ormap (lambda (prog-atual)
                 (FUNC prog-atual graf tail))
               lista-de-programas)
        (FUNC graf tail '())
        #f
        )
    )
  )

(define execute*
  (lambda(graf lista tail)
    (define command (list-ref lista 0))
    (if (execute*-rec graf lista tail command '() 50 0);
        (FUNC tail graf '())
        #f
        )
    )
  )

(define execute*-rec
  (lambda (graf lista tail command buffer limite-rec counter) ;observe que  aquivemos como apenas se fosse uma lista,mas eh  obuffer que estasendo modificado pela funcao acima.
    (if(cond ;esss cond ta imcompleto
         [(symbol=? command '|;|)
          (executePV graf lista tail)]
         [(symbol=? command '|U|)
          (executeU)]
         [(symbol=? command '()) ;bem se nao tem proximo comando, nao tem tail
          (FUNC graf lista '())])
       #t
       (if (> limite-rec counter) ;limite de quantas repeticoes do que ta em volto por * seguidamente, ex: <list>* |Se executa <list>;<list>;...;<list> . o limite de <list>'s seguidos que testaremos  
           (begin
             (set! counter (+ counter 1))
             (if (null? buffer) ;;aqui vamos aumentando quantas vezes sao executadas
                 (set! buffer(append buffer lista ))
                 (set! buffer(append buffer '(|;|) lista ))
                 )
             (execute*-rec graf buffer tail command buffer limite-rec counter))
           #f ;se passar chegar a 50 retorna falso
           )
       )
    )
  )   

(define executa-atomico
  (lambda (graf label tail)
    (if(or-map label graf tail FUNC)
       #t
       #f)
    )
  )
  
(define executeParentese
  (lambda(graf lista tail)
    (define extraido (extrair-ex lista)) ;lista : <sub-lista> , posicao-de-)
    (define temp-tail (list-tail lista (second extraido)))
    (define real-tail (list-tail temp-tail 1));aqui estaremos criando a tail sem o comando
    (define sub-lista (first extraido))
    (if (null? temp-tail)
        (FUNC graf sub-lista '());soh roda
        (cond
          [(symbol=? (first temp-tail) '|;|)
           (executePV graf sub-lista real-tail)]
          [(symbol=? (first temp-tail) 'U)
           (executeU graf sub-lista real-tail)]
          [(symbol=? (first temp-tail) '*)
           (execute* graf sub-lista real-tail)]
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
    ;(define comando (second lista))
    (if (null? (cdr lista))
        (executa-atomico graf label tail)
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

(define encontra-U
  (lambda ()
    (void)))

