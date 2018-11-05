#lang racket

;LEIA
;Aqui serah o lugar onde poderemos brincar e aprender racket,serve  qualquer coisa aqui
;Faca anotacoes do que quis fazer para o outro entender e aprender com o que vc fez
;Oque deu  certo OU NAO , soh para entendermos melhor

(define a '(1 2 3 4 5))

(define b '(10 10 10 10 10))

;testando car,qeu pega o cabecote e o resto (que como eh  visto como um par) eh o RESTO DA LISTA (pq a lista inteira eh um objeto)
(define add-firsts
  (lambda (lista1 lista2)
    (+ (car lista1) (car lista2))))

;esse aqui  ta  errado pq nao  da pra SOMAR listas
(define add-lasts
  (lambda (lista1 lista2)
    (+ (cdr lista1) (cdr lista2))))

(define tempL '())

;soma de duas listas (obs o tempL que ta ai nao tem haver com o definido  ai em cima,ele soh eh uma forma de chamarmos uma lista  vazia
;mas podiamos soh chamar '() que daria no mesmo teste (addL tempL a b) & (addL '() a b)
;Perceba o uso de cdr para pegar o resto da lista,enquanto a soma foi posta na tempL logo acima
(define addL
  (lambda (tempL list1 list2)
    (if (or (null? list1) (null? list2))
        tempL
        (let ([newTemp (append tempL (list (add-firsts list1 list2)))])
              (addL newTemp (cdr list1) (cdr list2))))))

;mesma cosia do de cima mas crieio que eh  recursivo la dentro o que da mais liberdade
(define addL2
  (lambda (list1 list2)
    (define recursivo
      (lambda (tempL list1 list2)
        (if (or (null? list1) (null? list2))
        tempL
        (let ([newTemp (append tempL (list (add-firsts list1 list2)))])
              (recursivo newTemp (cdr list1) (cdr list2))))))
    (recursivo '() list1 list2)))


(define addL3
  (lambda (list1 list2)
    (define BANANA (list "Chupa o  bico do meu peito e mete com vontade"))
    (define recursivo
      (lambda (tempL list1 list2)
        (if (or (null? list1) (null? list2))
        tempL
        (let ([newTemp (append tempL (list (add-firsts list1 list2)))])
              (recursivo newTemp (cdr list1) (cdr list2))))))
    (recursivo BANANA list1 list2)
    )
  )


;as 10:37 descubro que tem algo que  faz um procedimento para cada membro de uma lista...cool
(define addL-elegante
  (lambda (list1 list2)
    (map (lambda (list1 list2)
           (+ list1 list2))
         list1 list2)
    )
  )

(define st-to-sy  ;String to symbol-List
  (lambda (string)
    (let([split (string-split string)])
      (map (lambda (atual)
             (string->symbol atual))
           split)
      )
    )
  )

;SET! PARA ATUALIZAR VALORES
(define tm
  (lambda (test bola)
    (begin 
      (set! test (- test 1))
      (set! bola (+ bola 7))
      (print (list test bola))
      )
    )
  )


(define stringP " ( a ; b ( c U d ) ) ") 
(define listaP (st-to-sy stringP))


;Um contador que quando ve um "(" adiciona quando ve um ")" diminui,quando chega em 0 ele faz a FUNCAO com o q tinha la dentro


(define parenteses
  (lambda (list-sym FUNC)
    (writeln "COMECO FUNCAO Parenteses em:")
    (writeln list-sym)
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
                    (writeln counter)
                    (if (positive? counter) ;se 0 iremos fazer a  funcao, e nunca vai ser 0 sem ter tirodum ( antes, a nao  ser que apenas nao tenha
                        (set! buffer (append buffer (list s-atual)))
                        (FUNC buffer))]
                   [else (set! buffer (append buffer (list s-atual)))]
                   )
                 )
               list-sym)
      )   
    )
  )


(define sec-list (list  '|(| 'a  '|;| 'b '|)| '|;| 'c ))

(define second?
  (lambda (list)
    (if (null? (cdr list))
        #f
        (list-tail list 1))
    )
  )


(define not-void?;umaprocedure que faz  o oposto de
  (lambda (any)
    (if (void? any)
        #f
        #t)
    )
  )

(define part1-e-p
  (lambda (list-sym)
    (let ([counter 0]
          [buffer '()]
          [parentese-pos 0])
          (map (lambda (s-atual)
                 (cond
                   [(symbol=? s-atual '|(| )
                    (if (positive? counter) ;em ambos os casos (ser >0 ou nao) iremos incrementar
                        (begin
                          (set! buffer (append buffer (list s-atual)))
                          (set! counter (+ counter 1))
                          (set! parentese-pos (+ parentese-pos 1)))
                        (begin
                         (set! counter (+ counter 1))
                         (set! parentese-pos (+ parentese-pos 1)))
                        )]
                   [(symbol=? s-atual '|)| )
                    (set! counter (- counter 1))
                    (set! parentese-pos (+ parentese-pos 1))
                    (if (positive? counter) ;se 0 iremos fazer a  funcao, e nunca vai ser 0 sem ter tirodum ( antes, a nao  ser que apenas nao tenha
                        (set! buffer (append buffer (list s-atual)))
                        (list buffer parentese-pos))]
                   [else (begin
                           (set! parentese-pos (+ parentese-pos 1))
                           (set! buffer (append buffer (list s-atual))))]
                   )
                 )
               list-sym)
      )   
    )
  )

(define extrair;extrair parenteses | Sim precisa das operacoes abaixo para ficar uma lista bonitinha
  (lambda (list-sym)
     (define resp (filter not-void? (part1-e-p list-sym)))
    (define position (- (list-ref (car resp) 1) 1))
    (list (caar resp) position)
    )
  )



(define temp-list (member '|)| sec-list))



(define goo (list-ref sec-list (list-ref (extrair sec-list) 1)))

