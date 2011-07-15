A trie is a set of sequences. We represent it by a structure containing:

* a flag `has-nil?` telling whether the empty sequence is in the set, and
* an `a-list` mapping each different `car` of all sequences in the set
  to the trie for all the `cdr`s of the sequences with that `car`
.
Here's an example using a trie to find misspellings; that is, to find all words not in a dictionary:

    (define adjoin! (trie-method trie-adjoin! char=? list-walker))
    (define member? (trie-method trie-member? char=? list-walker))

    (define (install-dictionary! trie port)
    (for-each-input-line (lambda (word)
                           (adjoin! trie (string->list word)))
                         port))

    (define words (make-empty-trie))

    (call-with-input-file "/usr/dict/words"
    (lambda (port) (install-dictionary! words port)))

    (define (write-misspellings port)
    (for-each-input-word
     (lambda (word)
       (if (not (member? words
                         (map char-downcase (string->list word))))
           (writeln word)))
     port))

Hopefully that's all clear except for `list-walker`. What's that for?
I said a trie is a set of sequences, but a sequence doesn't have to be
represented as a list. Thus we supply a ‘walker’ to enumerate the
elements of a sequence in order. `list-walker` enumerates the elements
of a list, while `tree-walker` enumerates a different sequence for each
different tree of cons-pairs. This is useful if you want to index
arbitrary Scheme terms.

You can also write new walkers in the same style.

See also my [Scheme data structures package][].

[Scheme data structures package]: http://wry.me/~darius/software/scheme-data-structures.tar.gz

© 1994-2003 by Darius Bacon
