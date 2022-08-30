#lang plai
#| Precondiciones:
   Poscondiciones:
   NOMBRE: |#

#|(1) Precondiciones: Una lista heterogenea, un predicado.
   Poscondiciones: Una lista homogenea con los elementos solicitados.
    Regresa '() en caso que la lista pasada sea vacia o no haya ningun elemento esperado en la lista.
   filtra-lista: (listof any) procedure -> (listof any) |#
(define (filtra-lista ls p)
    (if (empty? ls)
        '() ;; then1
        (if (p (car ls)) ;; else1 begin
            (cons (car ls) (filtra-lista (cdr ls) p)) ;; then2
            (filtra-lista (cdr ls) p) ;; else2
        ) ;; else1 end
    )
)

#|(2) Precondiciones: Una lista heterogenea
   Poscondiciones: Una lista con el nombre del tipo de valor por cada elemento de la lista dada.
    Regresa '() en caso que la lista sea vacia.
   tipos-lista: (listof any) -> (listof string) |#
(define (tipos-lista ls)
    ;; Metodo auxiliar que decide de que tipo es el elemento que le pasemos como precondicion
    ;; tipo-elem: any -> string
    (define (tipo-elem a)
        (cond
            [(boolean? a) "boolean"]
            [(number? a) "number"]
            [(char? a) "char" ]
            [(string? a) "string"]
            [(symbol? a) "symbol"]
            [(keyword? a) "keyword"]
            [(pair? a) "pair"]
            [(list? a) "list"]
            [else "otro"]
        )
    )
    (if (empty? ls)
        empty
        (cons (tipo-elem (car ls)) (tipos-lista (cdr ls)))
    )
)

#|(3) Precondiciones: Un numero natural.
   Poscondiciones: Un booleano que indica si es numero raro (true) o no lo es (false)
   raro?: number -> boolean |#
(define (raro? n)
    ;; Metodo auxiliar que suma los digitos de un numero elevado a la longitud del mismo.
    ;; Precondiciones: El numero, la longitud del numero para su exponencial
    ;; Poscondiciones: La suma
    ;; suma-numeros: number number -> number
    (define (suma n expo) ;;begin suma
        (if (< n 10)
            n
            (+ (expt (remainder n 10) expo) (suma (quotient n 10) expo))
        )
    ) ;;end suma
    (let* (  ;;variables
            [numeroString (number->string n)]
            [longitud (string-length numeroString)]
            )
        ;; procedimiento
        (equal? n (suma n longitud))
    )
)

#|(4) Precondiciones: Una serie de numeros, potencialmente vacia
   Poscondiciones: Un booleano que indica si si la serie numerica dada es descendente o no
   descendente?: number* -> boolean |#
(define descendente? 
    (lambda nums
        (if (< (length nums) 2)
            #t
        (if (> (first nums) (second nums))
            (apply
                descendente?
                (cdr nums)
            )
            #f
        )
        )
    )    
)

#|(5) Precondiciones: Una cadena, potencialmente vacia
   Poscondiciones: Un booleano que dice si es palindromo (true) o no (false)
   palindromo?: string -> boolean |#
(define (palindromo? a)
    (cond
        [(equal? "" a) #t]
        [(< (string-length a) 2) #t]
        [else
            (if (equal? (string-ref a 0) (string-ref a (sub1 (string-length a))))
                (palindromo? (substring a 1 (sub1 (string-length a))))
                #f
            )
        ]
    )

)

#|(6) Precondiciones: Un numero entero
   Poscondiciones: Un booleano que dice si es primo (true) o no (false)
   primo?: number -> boolean |#
(define (primo? n)
    ;;Metodo auxiliar que te dice true si el numero es primo, y false en caso de que no lo sea.
    (define (modulo2 n m)
        (let* ( ;;variables
                [i (sub1 n)]
              )
        ;;operaciones
            (if (= n 1)
                #t
                (if (= m 1)
                    #t
                    (if (= (remainder n i) 0)
                        #f
                        (modulo2 n (sub1 i))
                    )
                )
            )
        )
    )
    (modulo2 n 0)
)

#|(8) Precondiciones: Una lista numérica.
   Poscondiciones: Un primer número que representa el promedio de los elementos en la lista, un segundo
   número que representa la moda y un último número que representa la mediana. |#
(define (prom-mod-med ls)
  (let* ( ;;variables
         [l (length ls)]
         )
    ;;Metodo auxiliar que saca el promedio
    (define (promedio ls)
      (

;; Metodo auxiliar que convierte un numero a string con funcion number->string
(define (convierte num)
    (if (number? num)
        (number->string num)
        "ERROR No es numero"
    )
)