#lang racket

;------------------------------------------------Guardado caso  precisemos consultar----------------------------------


#| Esta funcao foi substituida pela or-map por tratarmoso comando  de forma  atomica,e precisamos de umafuncao que retorne #t ou #f

(define pont-virg ;Assume que seja um atomico e um ; em seguida
  (lambda (graf comando FUNC)
    (define label-holder (first comando))
    (define tail (list-tail comando 2))
    (define validos (caminhos-validos (grafo-no-atual graf)(grafo-vetor-arestas graf)label-holder))
    (if (null? validos)
        (#f)
        (map (lambda (valid)
               (FUNC (grafo (grafo-vetor-arestas graf)(second valid)) tail))
              validos)
        )
    )
  )|#



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



;note que essa eh uma  lsita  dearestas,nao tivemos que definir nada,pq nao precisa ter um OBJETO
;Assim ta definidopelomenos  um grafo que podemos usar como testes
;(define entrada (list (list (list 'A 'B 'a) (list 'A 'C 'b) (list 'C 'D 'c)) 'A))

(define entrada (list (list "A B a"  "B C b" "C A c") 'A))

         
;aqui to usando string simplesmente pq permite ; mas podemos usar listas e  usar outro  simbolo para  fazer o ; 
(define pdl-teste1 "a ; b ; c")
(define pdl-teste2 "( a U b ) ; c")
(define pdl-teste3 "( a ) *")

(define t1 (st-to-sy pdl-teste1))
(define t2 (st-to-sy pdl-teste2))
(define t3 (st-to-sy pdl-teste3)) 

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


(define e-grafo (build-grafo entrada))


;Temos que definir quais seram o PASSOS para resolver nosso problema, para ai conseguirmos quebrar ele em pedacos menores
(define caminhos-validos
  (lambda (origem vetor-arestas label)
    (vector-filter (lambda (aresta)
                     (and (symbol=? (vector-ref aresta 0) origem) (symbol=? (vector-ref aresta 2) label))) 
                   vetor-arestas)
    )
  )



;versao or-map para o trabalho
;
(define or-map
  (lambda (label graf tail FUNC)
    (define resp #f)
    (define validos (caminhos-validos (grafo-no-atual graf) (grafo-vetor-arestas graf) label)) ;; isso eh um vector
    (vector-map (lambda (valid)
                  (if(FUNC (grafo (grafo-vetor-arestas graf)(vector-ref valid 2)) tail '()) ;irah testar a label ,andar no grafo eseguir em frente para cada caminho possivel
                     (begin
                       (set! resp #t)
                       (vector-set! valid 3 #t)) ;se tiver tudo certo aqui,isso marcarah as arestas passadas
                     (fprintf (current-output-port) ;Caso contrario aqui imprimiremos onde  deu problema
                              "Algo deu errado aqui: ~a , no ponto : ~a "
                              valid
                              (list (grafo-no-atual graf)))
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
                  (if (last atual)
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

