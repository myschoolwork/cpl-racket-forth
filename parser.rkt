#lang racket
(require "language.rkt")

(provide parse-expr)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(define (parse-expr src in)
  (define-values (line column position) (port-next-location in))
  (define next-char (read-char in))

  (define (decorate sexp span) ; enhance an s-expression with info so that it becomes a valid syntax object
    (datum->syntax #f sexp (list src line column position span)))
  
  
  ;; your implementation of the parser
  ;; read from `in` and use your implementation of `forth-lexer` to transform input characters into s-expressions
  ;; EDIT ME

  ;(let ([char (next-char)])
  ;  (cond
  ;    [eof-object? char (eof)]
  ;    []))
  (#t) ;so it compiles
)


; Define regular expressions for unsigned floats
; DON'T EDIT
(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))

; DON'T EDIT
(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

; Define some abbreviations that can be used within the lexer
; DON'T EDIT
(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (identifier-characters (re-or (char-range "A" "z")
                                "?" "!" ":" "$" "%" "^" "&"))
  (identifier (re-+ identifier-characters)))


; Extend the forth-lexer
; See http://docs.racket-lang.org/parser-tools/Lexers.html
; for more information about lexer
; EDIT ME
(define forth-lexer
  (lexer
   [#\+ `(plus)] ; return a s-expression of the `+` sign
   [#\- `(min)]
   ; ...
   ["dump" `(dump)]
   [number10 `(num ,(string->number lexeme))]
   [whitespace (forth-lexer input-port)])) ; ignore whitespace and advance the reader
