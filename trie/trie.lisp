;;; By Darius Bacon; public domain.

(define (assoc:test =? x a-list)   (cond ((null? a-list) #f)
                                         ((=? x (caar a-list)) (car a-list))
                                         (else (assoc:test =? x (cdr a-list)))))

(define-mutable-record trie (has-nil? a-list))

(define (make-empty-trie)          (make-trie #f '()))

(define (trie-empty? trie)         (and (not (trie.has-nil? trie))
                                        (null? (trie.a-list trie))))

(define (trie-prefixes trie)       (map car (trie.a-list trie)))

(define (trie-method operation =? walk-policy)
  (operation (lambda (extend? result-fn)
               (let ((step (trie-stepper extend? =?)))
                 (define (walking trie thing)
                   (and trie (walk-policy walking step trie thing)))
                 (lambda (trie thing)
                   (result-fn (walking trie thing)))))))

(define (trie-member? k) (k #f (lambda (trie) (and trie (trie.has-nil? trie)))))
(define (trie-adjoin! k) (k #t (lambda (trie) (set-trie.has-nil?! trie #t))))

(define (list-walker walk step trie xs)
  (cond ((null? xs) trie)
        (else (walk (step trie (car xs)) (cdr xs)))))

(define cons-marker (list 'cons))

(define (tree-walker walk step trie tree)
  (cond ((not (pair? tree)) (step trie tree))
        (else (walk (walk (step trie cons-marker) (car tree))
                    (cdr tree)))))

(define (trie-stepper extend? =?)
  (lambda (trie x)
    (cond ((assoc:test =? x (trie.a-list trie)) => cdr)
          (extend? (let ((new (make-empty-trie)))
                     (set-trie.a-list! trie (acons x new (trie.a-list trie)))
                     new))
          (else #f))))
