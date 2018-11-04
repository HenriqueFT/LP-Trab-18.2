#lang racket


#|
(define build-grafo
  (lambda (entrada)
    (grafo ((map (lambda (atual)
           (aresta (list-ref atual 0)(list-ref atual 1)(list-ref atual 2)))
         (list-ref entrada 0)))
      (list-ref entrada 1))))
|#

;(struct aresta (ori des label))

(struct grafo (lista-arestas no-atual))

;note que essa eh uma  lsita  dearestas,nao tivemos que definir nada,pq nao precisa ter um OBJETO
;Assim ta definidopelomenos  um grafo que podemos usar como testes
;(define entrada (list (list (list 'A 'B 'a) (list 'A 'C 'b) (list 'C 'D 'c)) 'A))

(define entrada (list (list "A B a"  "A C b" "C D c") 'A))

         
;aqui to usando string simplesmente pq permite ; mas podemos usar listas e  usar outro  simbolo para  fazer o ; 
(define pdl-teste1 "( a ; b ) ; c")
(define pdl-teste2 "( a U b ) ; c")
(define pdl-teste3 "( a ) *")


(define build-grafo
  (lambda (entrada)
    (grafo (map (lambda (atual)
                  (st-to-sy atual))
                (first entrada))
           (second entrada))))





#|
(a;b)Uc
lidando com U
(if (or (funcao (grafo "(a;b)") (funcao(grafo "c")))
'( ( a ; b ) )

lidando com ;

(funcao(grafo "(a;(bUc;d))")

(if (funcao (grafo "a"))
    (funcao (grafo <tudo depois do ";">))
    #\f))

|#


(define st-to-sy  ;String to symbol-List
  (lambda (string)
    (let([split (string-split string)])
      (map (lambda (atual)
             (string->symbol atual))
           split)
      )
    )
  )
    

;Temos que definir quais seram o PASSOS para resolver nosso problema, para ai conseguirmos quebrar ele em pedacos menores

(define caminhos-validos
  (lambda (origem label lista-arestas)
    (filter (lambda (aresta)
              (and (symbol=? (first aresta) origem) (symbol=? (third aresta) label))) 
              lista-arestas)))


             
