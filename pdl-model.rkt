#lang racket

;-------------------------------------------------------Primeiramente entradas e testes-----------------------------------------------------------

;note que essa eh uma  lsita  dearestas,nao tivemos que definir nada,pq nao precisa ter um OBJETO
;Assim ta definidopelomenos  um grafo que podemos usar como testes
;(define entrada (list (list (list 'A 'B 'a) (list 'A 'C 'b) (list 'C 'D 'c)) 'A))

(define grafo-entrada (list (list "A B a" "B C b" ) 'A)) ;"B C b" "C A c"

(define pdl-string "a ; b")
         
;aqui to usando string simplesmente pq permite ; mas podemos usar listas e  usar outro  simbolo para  fazer o ; 
(define pdl-teste1 "a ; b ; c")
(define pdl-teste2 "( a U b ) ; c")
(define pdl-teste3 "( a ) *")


;------------------------------------------------Aqui em cima colocaremos funcoes de apoio a resolucao--------------------------------------------


(struct grafo (vetor-arestas no-atual))


(define st-to-sy  ;String to symbol-List
  (lambda (string)
    (let* ([split (string-split string)]
           [list (map (lambda (atual)
                        (string->symbol atual))
                      split)])
      list)
    )
  )

(define build-grafo
  (lambda (entrada)
    (define arestas (map (lambda (atual)
                           (st-to-sy atual))
                         (first entrada))
      )
    (define arestas-com-booleana (map (lambda (atual) ; essa parte deve ser tirada caso nao utilizemos booleanas para  arestas percorridas
                                        (append atual (list #f))
                                        )
                                      arestas)
      )
    (define arestas-vetores (map(lambda (atual)
                                  (list->vector atual)
                                  )
                                arestas-com-booleana) ;trocariaa aqui por arestas caso nao usemos booleanas
      )
    (define graf-resp (grafo (list->vector arestas-vetores) (second entrada)));no caso de nao ter booleana coloque graf aqui | opara garantir deixei arestas-vector como um vetor de vetores
    graf-resp
    )
  )


;------------pequenos testes----------------
(define t1 (st-to-sy pdl-teste1))
(define t2 (st-to-sy pdl-teste2))
(define t3 (st-to-sy pdl-teste3)) 



(define e-programa(st-to-sy pdl-string))
(define e-grafo (build-grafo grafo-entrada))
;------------pequenos testes----------------


(define not-void?;umaprocedure que faz  o oposto de void? , feita na necessidade de filtrarmos voids
  (lambda (any)
    (if (void? any)
        #f
        #t)
    )
  )

(define part-extrair-parenteses; parte principal de  extracao de parenteses,porem retorna varios #voids dentro | Nao eficiente
  (lambda (list-sym)
    (let ([counter 0]
          [buffer '()])
      (map (lambda (s-atual)
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
               [else (set! buffer (append buffer (list s-atual)))]
               )
             )
           list-sym)
      )
    )
  )

(define extrair;extrair parenteses | Sim precisa das operacoes abaixo para ficar uma lista bonitinha
  (lambda (list-sym)
    (car(filter not-void? (part-extrair-parenteses list-sym)))
    )
  )

(define extrair-ex; retorna uma lista que a primeira  posicao eh uma lista normal qeu seria retornada em extrair e tambema posicao de ")"
  (lambda (list-sym)
    (define resp (filter not-void? (part-extrair-parenteses list-sym)))
    (define position (+ (length (car resp)) 1)) ;;soma 1 a maispor  isso tenho que colocar essa loucura
    (list (car resp) position)
    )
  )


;Temos que definir quais seram o PASSOS para resolver nosso problema, para ai conseguirmos quebrar ele em pedacos menores
(define caminhos-validos
  (lambda (origem vetor-arestas label)
    (vector-filter (lambda (aresta)
                     (and (symbol=? (vector-ref aresta 0) origem) (symbol=? (vector-ref aresta 2) label))) 
                   vetor-arestas)
    )
  )


;versao or-map para o trabalho
(define or-map
  (lambda (label graf tail FUNC)
    (define resp #f)
    (define validos (caminhos-validos (grafo-no-atual graf) (grafo-vetor-arestas graf) label)) ;; isso eh um vector
    (vector-map (lambda (valid)
                  (vector-set! valid 3 #t) 
                  (set! graf (grafo (grafo-vetor-arestas graf)(vector-ref valid 2)))
                  (if(FUNC graf tail '()) ;irah testar a label ,andar no grafo eseguir em frente para cada caminho possivel
                     (begin
                       (set! resp #t)) ;se tiver tudo certo aqui,isso marcarah as arestas passadas
                     (begin
                       (vector-set! valid 3 #f)
                       (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
                              "Algo deu errado aqui: ~a , no ponto : ~a \n"
                              valid
                              (grafo-no-atual graf)))
                     )
                  )
                validos)
    resp)  ;valor de fato retornado #f ou #t
  )
  
;
;Checarah quais arestas nao foram percorridas no grafo (DEVERAHSER POSTA DEPOIS DE TODA EXECUCAO  DE FUNC)
(define nao-percorridas
  (lambda (graf)
    (define resp #t)
    (define caminhos (grafo-vetor-arestas graf))
    (vector-map (lambda (atual)
                  (fprintf (current-output-port)
                                 "Na aresta ~a temos se percorreu como: ~a \n"
                                 atual
                                 (- (vector-length atual) 1))
                  (if (vector-ref atual (- (vector-length atual) 1))
                      (void) ;se tiver #t nao faz nada
                      (begin
                        (set! resp #f);se tiver uma falsa retorna falsa. E a imprime
                        (fprintf (current-output-port)
                                 "Esta aresta nao foi percorrida: ~a \n"
                                 atual))
                      )
                  )
                caminhos)
    resp
    )
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
        )
    )
  )
      

(define encontra-U-void
  (lambda (sym-list)
    
    (let ([counter 0]
          [achado #f]
          [bufferzinho '()]
          [buffer '()])
      (begin
        (map (lambda (s-atual)
               (if achado
                   (void)
                   (cond
                     [(symbol=? s-atual '|(| )
                      (if (positive? counter) ;em ambos os casos (ser >0 ou nao) iremos incrementar
                          (begin
                            (set! bufferzinho (append bufferzinho (list s-atual)))
                            (set! counter (+ counter 1)))
                          (set! counter (+ counter 1)))
                      ]
                     [(symbol=? s-atual '|)| )
                      (set! counter (- counter 1))
                      (if (positive? counter) ;se 0 iremos fazer a  funcao, e nunca vai ser 0 sem ter tirado um ( antes, a nao  ser que apenas nao tenha
                          (set! bufferzinho (append bufferzinho (list s-atual)))
                          buffer)]
                     [(and (symbol=? s-atual 'U ) (zero? counter))
                      (begin
                        (set! buffer (append buffer (list bufferzinho)))
                        (set! bufferzinho (null) ))
                      ]
                     [(and (symbol=? s-atual '|;| ) (zero? counter))
                      (begin
                        (set! achado #t )
                        (set! buffer(append buffer (list bufferzinho)))
                        (set! bufferzinho (null)))

                      ]
                     [else (set! bufferzinho (append bufferzinho (list s-atual)))]
                     )
                   )
               )
             (list-tail sym-list 1)
  
             )
        (if (equal? achado true)
            (void)
             (set! buffer(append buffer (bufferzinho)))
         )
        )
    )
  ))

(define encontra-U
  (lambda (sym-list)
    (define retorno (encontra-PV sym-list))
    (define divide (encontra-U-void (first retorno)))
    (append divide (second retorno))
    ))
;----------------------------------------------------------------Aqui serah  o corpo principal da resolucao------------------------------------------------------


(define executePV ;executa  ; ,ou seja se o proximo comando era ; este serah tratado
  (lambda (graf lista tail)
    ;(define lista-sem-comando (list-tail lista 1))  
    (if(FUNC graf lista  tail)
       (FUNC graf tail '())
       #f))
  )

(define executeU ;executa  U ,ou seja se o proximo comando era U este serah tratado
  (lambda (graf lista tail)
    ;(define lista-sem-comando (list-tail lista 1))  
    (define lista-de-programas (encontra-U tail))
    (if (ormap (lambda (prog-atual)
                 (FUNC graf prog-atual tail))
               lista-de-programas)
        (FUNC graf tail '())
        #f
        )
    )
  )

(define execute* ;executa *  ,ou seja se o proximo comando era * este serah tratado
  (lambda(graf lista tail)
    (define command (list-ref lista 0))
    (if (execute*-rec graf lista tail command '() 50 0); 50 foi o valor  maximo de execussoes seguidas do que ta marcado por * que serao feitas
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
    ;as definicoes abaixo sao totalmente desncessessarias, apenas usadas como "apelidos"
    (define extraido (extrair-ex lista)) ;lista : <sub-lista> , posicao-de-)
    (define temp-tail (list-tail lista (second extraido)))
    (define real-tail (list-tail temp-tail 2));aqui estaremos criando a tail sem o comando
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
    (if (null? (cdr lista));caso nao tenha proximo comando
        (executa-atomico graf label tail)
        (cond
          [(symbol=? (second lista) '|;|)(executePV graf (list label) (list-tail lista 2))]
          [(symbol=? (second lista) 'U)(executeU graf (list label) (list-tail lista 2))]
          [(symbol=? (second lista) '*)(execute* graf (list label) (list-tail lista 2))]
          [else (if (or-map label graf tail FUNC)
                    #t
                    #f)]
          )
        )
    )
  )

(define FUNC
  (lambda (graf lista tail)
    (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
             "Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n ____________________________ \n"
             (grafo-vetor-arestas graf)
             (grafo-no-atual graf)
             lista
             tail)
    (if (null? lista) ;caso FUNC tenha a  instrucao sendo uma lista '() ,quer dizer que chegou no final,entao reetorna #t
        #t
        (if (symbol=? (car lista) '|(| )
            (executeParentese graf lista tail);aqui executaremos tudo caso tenha ( na frente
            (executeLetra graf lista tail)
            ) 
        )
    )
  )

(define run
  (lambda ()
    (writeln (FUNC e-grafo e-programa '()))
    )
  )

(run)