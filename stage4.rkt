#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (longest-common-prefix-iter prefix w1 w2)
    (cond
      ((or (null? w1) (null? w2)) (append (list (reverse prefix)) (list w1) (list w2)))
      ((equal? (car w1) (car w2)) (longest-common-prefix-iter (cons (car w1) prefix) (cdr w1) (cdr w2)))
      (else (append (list (reverse prefix)) (list w1) (list w2)))))
  (longest-common-prefix-iter null w1 w2))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (define (longest-common-prefix-of-list-iter prefix words)
    (cond
      ((collection-empty? words) prefix)
      (else (longest-common-prefix-of-list-iter
             (car (longest-common-prefix prefix (collection-first words))) (collection-rest words)))))
  (longest-common-prefix-of-list-iter (collection-first words) (collection-rest words)))


(define (match-pattern-with-label st pattern)
  ;; ramura cu prima litera a etichetei egala cu prima litera a pattern-ului
  (define branch (get-ch-branch st (car pattern)))

  ;; rezultatul cautarii celui mai lung prefix comun intre eticheta ramurii
  ;; si pattern
  (define res (cond
                (branch (longest-common-prefix pattern (get-branch-label branch)))
                (else branch)))
  
  (cond
    (branch (cond
              ((null? (cadr res)) #t)
              ((null? (caddr res)) (list (car res) (cadr res) (get-branch-subtree branch)))
              (else (list #f (car res)))))
    (else (list #f null))))


(define (st-has-pattern? st pattern)
  ;; rezultatul cautarii celui mai lung prefix comun intre eticheta ramurii
  ;; si pattern
  (define res (match-pattern-with-label st pattern))

  (cond
    ((list? res) (cond
                   ((car res) (st-has-pattern? (caddr res) (cadr res)))
                   (else #f)))
    (else res)))


(define (get-suffixes text)
  (cond
    ((null? text) empty-collection)
    (else (collection-cons text (get-suffixes (cdr text))))))


(define (get-ch-words words ch)
  (collection-filter (compose not null?) (collection-map (λ (word) (if (equal? (car word) ch) word null))
                                                         (collection-filter (compose not null?) words))))



(define (ast-func suffixes)
  (cond
    ((collection-empty? suffixes) empty-collection)
    (else (cons (list (car (collection-first suffixes)))
                (collection-map (λ (word) (cdr word)) suffixes)))))


(define (cst-func suffixes)
  (define prefix (longest-common-prefix-of-collection suffixes))
  (cons prefix (collection-map (λ (suffix) (drop suffix (length prefix))) suffixes)))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (define sorted-words (collection-map (λ (letter) (get-ch-words suffixes letter)) alphabet))
  (define sorted-trees (collection-map labeling-func (collection-filter (compose not collection-empty?) sorted-words)))
 
  (collection-map (λ (subtree) (cons (get-branch-label subtree)
                                             (suffixes->st labeling-func (get-branch-subtree subtree) alphabet)))
                          sorted-trees))


; nu uitați să convertiți alfabetul într-un flux
(define (text->st text)
  (define (collection-alphabet alph)
    (cond
      ((null? alph) empty-collection)
      (else (collection-cons (car alph) (collection-alphabet (cdr alph))))))
  

  (let* ((complete-text (append text '(#\$))) (alphabet (collection-alphabet (sort (remove-duplicates complete-text) char<?))))
    (λ (labeling-func) (suffixes->st labeling-func (get-suffixes complete-text) alphabet))))


(define (text->ast text)
  ((text->st text) ast-func))


(define (text->cst text)
  ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let loop ((st (text->cst text)) (need-len len) (result '()))
    (cond ((st-empty? st) #f)
          ((<= need-len 0) (take result len))
          (else
           (let* ((branch (first-branch st)) (label (get-branch-label branch)) (subtree (get-branch-subtree branch)))
             (or (loop subtree (- need-len (length label)) (append result label))
                 (loop (other-branches st) need-len result)))))))