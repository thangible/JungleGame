#lang racket

(define (type-of x)
  
  (if (procedure? x)
      "procedure"
      (if(boolean? x)
         "boolean"
          (if (pair? x)
              "pair"
              (if(list? x)
                 "list"
                 (if (symbol? x)
                     "symbol"
                     (if(number? x)
                        "number"
                        (if (char? x)
                            "char"
                            (if(string? x)
                               "string"
                               (if (vector? x)
                                   "vector"
                                   "nothing"
  ))))))))))


( type-of (+ 3 7) )
( type-of type-of )
( type-of ( type-of type-of ) )
( type-of ( string-ref " Schneewitchen und die 7 Zwerge " 2) )
( type-of ( lambda ( x ) x ) )
( define ( id z ) z )
( type-of ( id cos ) )
( type-of '(1 2 3) )
( type-of '() )







  