;; Døme på funksjonell programmering med scheme

;ein enkel prosedyre som returner ein ny prosedyre som holder rede på kor
;mange gonger den har blitt kalla
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))


;Ouroboros
(define (make-ring liste)
  ;; ein prosedyre som lager ein ring; Neste element etter siste er fyrste.
  (let ((slange liste))
    (let halespising ((slange liste))
      (if (null? (cdr slange)) (set-cdr! slange liste)
          (halespising  (cdr slange))))
    )
      (define toppen (car liste))

 ;; Roterar ringen, slik at hodet endrer seg
  (define (right-rotate!)
    (let ((ouro liste))
      (let høgring ((ouro liste))
        (if (eq? liste (cdr ouro)) (set! liste ouro)
            (høgring (cdr ouro)))))
    (set! toppen (car liste))
    )

  (define (left-rotate!)
    (set! liste (cdr liste))
    (set! toppen (car liste))
    (display toppen))

      
  ;Sletting. Går til høgre, og setter peikaren til å peike på det som er bortenfor det som var hodet
  ; flytter så liste til venstre igjen
  
  (define (delete!)
    (cond ((eq? liste (cdr liste)) (begin
                                     (set-car! liste '() )
                                     (set! toppen (car liste))
                                     (top)))
          (else (hjelp!)
    )))

  (define (hjelp!)
    (right-rotate!)
    (define y liste)
    (set-cdr! y (cddr liste))
    (left-rotate!))

  (define (top)
    (display toppen))

  (define (insert! inn)
    (cond ((eq? (car liste) '()) (begin (set-car! liste (car inn)) (set! toppen (car liste)) (display toppen)))
          (else (begin
                  (right-rotate!)
                  (set-cdr! liste (cons (car inn) (cdr liste)))
                  (left-rotate!)))))
          

;; litt pynting
  (define (msg m . inn)
    (cond ((eq? m 'top) (top))
          ((eq? m 'høgre) (right-rotate!) (top))
          ((eq? m 'venstre) (left-rotate!))
          ((eq? m 'slett) (delete!))
          ((eq? m 'test) (testings))
          ((eq? m 'putin) (insert! inn))
          (else (display "det skjer?"))))
  msg) 


(define (top ouroboros)
  (ouroboros 'top))

(define (right-rotate! ouroboros)
  (ouroboros 'høgre))

(define (left-rotate! ouroboros)
  (ouroboros 'venstre))

(define (delete! ouroboros)
  (ouroboros 'slett))

(define (insert! ouroboros x)
  (ouroboros 'putin x))
