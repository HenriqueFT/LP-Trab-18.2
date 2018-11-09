#lang racket

;-------------------------------------------------------Primeiramente entradas e testes-----------------------------------------------------------

;Cada aresta eh dada por um trio de "simbolos" separados  por um espaco " "
;A orde  que estao eh (Origem Destino Label).
;Qualquer combinacao de letras e numeros em sequencia podem representar qual quer um dos  3, porem soh devem haver 3 (por string) e devem ser separadas por espaco
;O ultimo simbolo eh '<ponto de origem> . Pode ser qualquer sequencia de caracteres,mas deve possuir um ' logo antes 

(define grafo-entrada (list (list  "A A a" "A B b" "B C c"  ) 'A)) 

;Dentro desta string conterah o programa a ser testado
;As regras acima permanessem, todo  caracter deve ser separado por um espaco. Isso inclui oscomandos ; U e * . Assim como os parenteses

(define pdl-string " ( a ; b ) * ; c *")

;grafo-exemplos
; "A A a" "A B b" 
;"A B a" "B C b" "C A c" 
;"A B a" "A C b" "C A c"

;pdl-string exemplos
; a ; b ; c
;( a ; b ; c ) *
;( a ; b ) * ; c 


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
    (define graf-resp (grafo (list->vector arestas-vetores) (vector (second entrada))));no caso de nao ter booleana coloque graf aqui | opara garantir deixei arestas-vector como um vetor de vetores
    graf-resp
    )
  )




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
    (define validos (caminhos-validos (vector-ref (grafo-no-atual graf) 0) (grafo-vetor-arestas graf) label)) ;; isso eh um vector
    (vector-map (lambda (valid)
                  (vector-set! valid 3 #t) 
                  ;(set! graf (grafo (grafo-vetor-arestas graf)(vector-ref valid 1)))
                  (vector-set! (grafo-no-atual graf) 0 (vector-ref valid 1))
                  (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
                           "EXECUTE OR-MAP \n Indo-para :~a\n _______________________________________________ \n"
                           valid)
                  (if(FUNC graf tail '()) ;irah testar a label ,andar no grafo eseguir em frente para cada caminho possivel
                     (begin
                       (set! resp #t)) ;se tiver tudo certo aqui,isso marcarah as arestas passadas
                     (begin
                       (vector-set! valid 3 #f)
                       (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
                                "Algo deu errado aqui: ~a , no ponto : ~a \n"
                                valid
                                (vector-ref (grafo-no-atual graf) 0)))
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
                  #|(fprintf (current-output-port)
                                 "Na aresta ~a temos se percorreu como: ~a \n"
                                 atual
                                 (- (vector-length atual) 1))|#
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

(define e-PV-void-support
  (lambda (buffer achado)
    (if achado
        buffer
        (begin
          (set! buffer (append buffer #f))
          buffer
          )
        )
    )
)

(define encontra-PV-void
  (lambda (sym-list)
    (write sym-list)
    (let ([counter 0]
          [achado #f]
          [buffer '()])
      
      (e-PV-void-support (map (lambda (s-atual)
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
                                       ]
                                      [else (set! buffer (append buffer (list s-atual)))]
                                      )
                                    )
                                )
                              (list-tail sym-list 1)
                              )
                         achado)
      )
    )
  )
  (define encontra-PV
    (lambda (sym-list)
      (define retorno (filter not-void? (encontra-PV-void sym-list)))
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
    (lambda (graf lista tail parentese)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE PV \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               lista
               tail)
      ;(define lista-sem-comando (list-tail lista 1))
      (if parentese
          (if(and (FUNC graf lista tail) (FUNC graf tail '()))
             #t
             #f)
          (if(FUNC graf lista tail)
             #t
             #f))
      )
    )

  (define executeU ;executa  U ,ou seja se o proximo comando era U este serah tratado
    (lambda (graf lista tail parentese)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE U \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               lista
               tail)
      ;(define lista-sem-comando (list-tail lista 1))  
      (define lista-de-programas (encontra-U tail))
      (if (ormap (lambda (prog-atual)
                   (FUNC graf prog-atual tail))
                 lista-de-programas)
          (if parentese
              (FUNC graf tail '())
              #t)
          #f
          )
      )
    )

  (define execute* ;executa *  ,ou seja se o proximo comando era * este serah tratado
    (lambda(graf lista tail parentese)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE * \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               lista
               tail)
      (define command 'b)
      (if(null? tail)
         (set! command '||)
         (set! command (list-ref tail 0)))
      (define novo-tail '())
      (if(null? tail)
         (set! novo-tail '())
         (set! novo-tail (list-tail tail 1)))
      (if (execute*-rec graf lista novo-tail command '() (vector-length (grafo-vetor-arestas graf)) 0 parentese); 50 foi o valor  maximo de execussoes seguidas do que ta marcado por * que serao feitas
          #t
          #f
          )
      )
    )

  (define execute*-rec
    (lambda (graf lista tail command buffer limite-rec counter parentese) ;observe que  aquivemos como apenas se fosse uma lista,mas eh  obuffer que estasendo modificado pela funcao acima.
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE * REC \n Comando: ~a \n Buffer : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               command
               buffer
               lista
               tail)
      (define real (cond 
           [(symbol=? command '|;|)
            (if parentese
            (executePV graf lista tail #t)
            (executePV graf lista tail #f))]
           [(symbol=? command '|U|)
            (if parentese
            (executeU graf lista tail #t)
            (executeU graf lista tail #f))]
           [else (if(symbol=? command '|| ) ;bem se nao tem proximo comando, nao tem tail
                    (FUNC graf lista '())
                    (void)) ]))
      (if real
         #t 
         (if (> limite-rec counter) ;limite de quantas repeticoes do que ta em volto por * seguidamente, ex: <list>* |Se executa <list>;<list>;...;<list> . o limite de <list>'s seguidos que testaremos  
             (begin
               (set! counter (+ counter 1))
               (if (null? buffer) ;;aqui vamos aumentando quantas vezes sao executadas
                   (set! buffer(append buffer lista ))
                   (set! buffer(append buffer '(|;|) lista ))
                   )
               (execute*-rec graf buffer tail command buffer limite-rec counter parentese))
             #f ;se passar chegar a 50 retorna falso
             )
         )
      )
    )   

  (define executa-atomico
    (lambda (graf label tail)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE ATOMICO \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               label
               tail)
      (if(or-map label graf tail FUNC)
         #t
         #f)
      )
    )
  
  (define executeParentese
    (lambda(graf lista tail)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE parentese \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               lista
               tail)
      ;as definicoes abaixo sao totalmente desncessessarias, apenas usadas como "apelidos"
      (define extraido (extrair-ex lista)) ;lista : <sub-lista> , posicao-de-)
      (define temp-tail (list-tail lista (+ (second extraido) 1)))
      (define real-tail (list-tail temp-tail 1));aqui estaremos criando a tail sem o comando
      (define sub-lista (first extraido))
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "\n LISTAS \n Extraido: ~a \n temp-tail: ~a \n real-tail : ~a \n sub-lista : ~a \n _______________________________________________ \n"
               extraido
               temp-tail
               real-tail
               real-tail)
      (if (null? temp-tail)
          (FUNC graf sub-lista '());soh roda
          (cond
            [(symbol=? (first temp-tail) '|;|)
             (executePV graf sub-lista real-tail #t)]
            [(symbol=? (first temp-tail) 'U)
             (executeU graf sub-lista real-tail #t)]
            [(symbol=? (first temp-tail) '*)
             (execute* graf sub-lista real-tail #t)]
            [else (if (FUNC graf (first extraido) '())
                      #t
                      #f)]
            )
          )
      )
    )

  (define executeLetra
    (lambda(graf lista tail)
      (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
               "EXECUTE LETRA \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf)0)
               lista
               tail)
      (define label (first lista))
      ;(define comando (second lista))
      (if (null? (cdr lista));caso nao tenha proximo comando
          (executa-atomico graf label tail)
          (cond
            [(symbol=? (second lista) '|;|)(executePV graf (list label) (list-tail lista 2) #f)]
            [(symbol=? (second lista) 'U)(executeU graf (list label) (list-tail lista 2) #f)]
            [(symbol=? (second lista) '*)(execute* graf (list label) (list-tail lista 2) #f)]
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
               "CHAMADA DE FUNC \n Grafo-arestas: ~a \n Grafo-no-atual : ~a \n Lista : ~a \n Tail : ~a\n _______________________________________________ \n"
               (grafo-vetor-arestas graf)
               (vector-ref (grafo-no-atual graf) 0)
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

  ;------------Preparacao----------------------
  (define e-programa(st-to-sy pdl-string))
  (define e-grafo (build-grafo grafo-entrada))
  ;------------Preparacao----------------------
  (define run
    (lambda ()
      (if(and (FUNC e-grafo e-programa '()) (nao-percorridas e-grafo))
         (writeln "DEU TUDO CERTO!!!")
         (writeln "Nao tivemos sucesso :/  ")
      )
    )
    )

  (run)