#lang racket


(struct grafo (lista-arestas no-atual))

;note que essa eh uma  lsita  dearestas,nao tivemos que definir nada,pq nao precisa ter um OBJETO
;Assim ta definidopelomenos  um grafo que podemos usar como testes
(define grafo-teste (grafo '('(A B a) '(A C b) '(C D c)) 'A))

;aqui to usando string simplesmente pq permite ; mas podemos usar listas e  usar outro  simbolo para  fazer o ; 
(define pdl-teste1 "(a;b);c")
(define pdl-teste2 "(aUb);c")
(define pdl-teste3 "(a)*")

;(define aresta (bola origem destino))

;Temos que definir quais seram o PASSOS para resolver nosso problema, para ai conseguirmos quebrar ele em pedacos menores

(define caminhos-validos
  (lambda



             
