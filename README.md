Screenfuls
==========

A ‘screenful’ is the programming analog of [nanofiction][]: a readable
program that does something interesting and fits in one screen.

This repository started from the [screenfuls][] project on [Darius
Bacon’s old web page][].

[nanofiction]: http://www.wunderland.com/WTS/Andy/Nanofiction.html
[screenfuls]: http://wry.me/~darius/hacks/screenfuls/screen3.html
[Darius Bacon’s old web page]: http://wry.me/~darius/

To do
-----

Write “build scripts” so that a person can run the programs easily.

Add more screenfuls.

Add one-line summaries of each program to this file.

Candidates
----------

Norvig’s [spelling corrector].

[spelling corrector]: http://norvig.com/spell-correct.html

    import re, collections

    def words(text): return re.findall('[a-z]+', text.lower()) 

    def train(features):
        model = collections.defaultdict(lambda: 1)
        for f in features:
            model[f] += 1
        return model

    NWORDS = train(words(file('big.txt').read()))

    alphabet = 'abcdefghijklmnopqrstuvwxyz'

    def edits1(word):
       splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
       deletes    = [a + b[1:] for a, b in splits if b]
       transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
       replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
       inserts    = [a + c + b     for a, b in splits for c in alphabet]
       return set(deletes + transposes + replaces + inserts)

    def known_edits2(word):
        return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

    def known(words): return set(w for w in words if w in NWORDS)

    def correct(word):
        candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
        return max(candidates, key=NWORDS.get)

Kragen’s bootstrapping [PEG parser generator][].

[PEG parser generator]: https://github.com/kragen/peg-bootstrap/blob/master/peg.md



The “[BNF in Forth][]” paper by Brad Rodriguez from around 1990.

[BNF in Forth]: http://www.bradrodriguez.com/papers/bnfparse.htm

                                                                  Scr #       3
     0 \ BNF Parser                                (c) 1988 B. J. Rodriguez
     1 0 VARIABLE SUCCESS
     2 : <BNF   SUCCESS @ IF  R> IN @ >R DP @ >R  >R
     3    ELSE  R> DROP  THEN ;
     4 : BNF>   SUCCESS @ IF  R>  R> R> 2DROP   >R
     5    ELSE  R>  R> DP ! R> IN !  >R THEN ;
     6 : |    SUCCESS @ IF  R> R> R> 2DROP DROP
     7    ELSE  R> R> R> 2DUP >R >R IN ! DP !  1 SUCCESS !  >R THEN ;
     8 : BNF:   [COMPILE] : SMUDGE COMPILE <BNF ; IMMEDIATE
     9 : ;BNF   COMPILE BNF> SMUDGE [COMPILE] ; ; IMMEDIATE
    10
    11 : @TOKEN ( - n)   IN @ TIB @ + C@ ;
    12 : +TOKEN ( f)    IF 1 IN +! THEN ;
    13 : =TOKEN ( n)    SUCCESS @ IF @TOKEN =  DUP SUCCESS ! +TOKEN
    14    ELSE DROP THEN ;
    15 : TOKEN ( n)    <BUILDS C, DOES> ( a)  C@ =TOKEN ;

                                                                  Scr#        4
     0 \ BNF Parser - 8086 assembler version       (c) 1988 B. J. Rodriguez
     1 0 VARIABLE SUCCESS
     2 CODE <BNF    -1 # SUCCESS #) TEST, NE IF,      \ if passing,
     3       4 # RP SUB,    0FDFE # W MOV, ( U ptr)   \     checkpoint
     4       ' IN @ [W]  AX MOV, AX 2 [RP] MOV,       \     and continue
     5       ' DP @ [W]  AX MOV, AX 0 [RP] MOV,
     6    ELSE, 0 [RP] IP MOV,  RP INC,  RP INC,      \  else, exit now!
     7    THEN, NEXT
     8
     9 CODE BNF>    -1 # SUCCESS #) TEST, EQ IF,      \  if failing,
    10        0FDFE # W MOV, ( U ptr)                  \     backtrack to
    11        0 [RP] AX MOV, AX ' DP @ [W] MOV,        \     checkpoint
    12        2 [RP] AX MOV, AX ' IN @ [W] MOV,
    13     THEN, 4 # RP ADD, NEXT                      \  discard checkpoint
    14                                                 \     and continue
    15

                                                                  Scr#        5
     0 \ BNF Parser - 8086 assembler version       (c) 1988 B. J. Rodriguez
     1 CODE |   -1 # SUCCESS #) TEST, NE IF,        \ if passing,
     2       4 # RP ADD,                            \   discard checkpoint
     3       0 [RP] IP MOV, RP INC, RP INC,         \   and exit now
     4    ELSE, 0FDFE # W MOV,                      \ else, backtrack,
     5       0 [RP] AX MOV,    AX ' DP @ [W] MOV,   \   leaving checkpoint
     6       2 [RP] AX MOV,    AX ' IN @ [W] MOV,   \   stacked, and
     7       SUCCESS #) INC,                        \   set true for next
     8    THEN, NEXT                                \   alternate
     9
    10
    11
    12
    13
    14
    15

                                                                  Scr #       6
     0 \ BNF Parser Example #1 - pattern recog.             18 9 88 bjr 19:41
     1 \ from Aho & Ullman, Principles of Compiler Design, p.137
     2 \ this grammar recognizes strings having balanced parentheses
     3
     4 HEX    28 TOKEN '('      29 TOKEN ')'      0 TOKEN <EOL>
     5
     6 BNF: <CHAR>     @TOKEN DUP 2A 7F WITHIN SWAP 1 27 WITHIN OR
     7    DUP SUCCESS ! +TOKEN ;BNF
     8
     9 BNF: <S>       '(' <S> ')' <S>   |   <CHAR> <S>   |   ;BNF
    10
    11 : PARSE     1 SUCCESS !    <S> <EOL>
    12   CR SUCCESS @ IF ." Successful " ELSE ." Failed " THEN ;
    13
    14
    15

                                                                  Scr#        7
     0 \  BNF Parser Example    #2  - infix notation        18 9 88 bjr 14:54
     1 HEX    2B TOKEN   '+'    2D  TOKEN '-'     2A  TOKEN  '*'     2F TOKEN '/'
     2        28 TOKEN   '('    29  TOKEN ')'     5E  TOKEN  '^'
     3        30 TOKEN   '0'    31  TOKEN '1'     32  TOKEN  '2'     33 TOKEN '3'
     4        34 TOKEN   '4'    35  TOKEN '5'     36  TOKEN  '6'     37 TOKEN '7'
     5        38 TOKEN   '8'    39  TOKEN '9'       0 TOKEN  <EOL>
     6
     7 BNF: <DIGIT>      '0'  | '1' | '2' |  '3' | '4' | '5' | '6' | '7'
     8     |  '8' | '9' ;BNF
     9 BNF: <NUMBER>    <DIGIT> <NUMBER>    |     <DIGIT> ;BNF
    10
    11
    12
    13
    14
    15

                                                                  Scr#        8
     0 \ BNF Parser Example     #2 - infix notation         18 9 88 bjr 15:30
     1 \ from Aho & Ullman,     Principles of Compiler Design, pp.135,178
     2 : [HERE]     HERE 0 ,   -2 CSP +!  ;    IMMEDIATE
     3
     4 BNF:   <ELEMENT>     '(' [HERE]  ')'  |    <NUMBER> ;BNF
     5 BNF:   <PRIMARY>     '-' <PRIMARY>    |   <ELEMENT> ;BNF
     6 BNF:   <FACTOR>    <PRIMARY> '^' <FACTOR> | <PRIMARY> ;BNF
     7 BNF:   <T'>     '*' <FACTOR> <T'> | '/' <FACTOR> <T'> ;BNF
     8 BNF:   <TERM>    <FACTOR> <T'> ;BNF
     9 BNF:   <E'>     '+' <TERM> <E'> | '-' <TERM> <E'>    ;BNF
    10 BNF:  <EXPRESSION>      <TERM> <E'> ;BNF
    11 ' <EXPRESSION> CFA SWAP !      \ fix the recursion in <ELEMENT>
    12
    13 : PARSE     1 SUCCESS !     <EXPRESSION> <EOL>
    14    CR SUCCESS @ IF  ." Successful " ELSE ." Failed " THEN ;
    15

                                                                  Scr #       9
     0  \ BNF  Example #3      code generation               18 9 88 bjr 21:57
     1 HEX    2B TOKEN   '+'    2D  TOKEN '-'     2A  TOKEN  '*'     2F TOKEN'/'
     2        28 TOKEN   '('    29  TOKEN ')'     5E  TOKEN  '^'
     3        30 TOKEN   '0'    31  TOKEN '1'     32  TOKEN  '2'     33 TOKEN '3'
     4        34 TOKEN   '4'    35  TOKEN '5'     36  TOKEN  '6'     37 TOKEN '7'
     5        38 TOKEN   '8'    39  TOKEN '9'       0 TOKEN  <EOL>
     6
     7 BNF: {DIGIT}      '0'  | '1' | '2' |  '3' | '4' | '5' | '6' | '7'
     8     |  '8' | '9' ;BNF
     9 BNF: <DIGIT>       @TOKEN {DIGIT} C, ;BNF
    10
    11 BNF: <NUMBER>      <DIGIT> <NUMBER>    |    <DIGIT> ;BNF
    12
    13 : (,")    R COUNT DUP 1+ R> + >R       HERE SWAP DUP ALLOT CMOVE ;
    14 : ,"    COMPILE (,") 22 WORD HERE       C@ 1+ ALLOT  ;    IMMEDIATE
    15

                                                                  Scr#       10
     0 \  BNF Example #3       code generation               18 9 88 bjr 21:57
     1 :  [HERE]     HERE 0  ,   -2 CSP +!  ;    IMMEDIATE
     2
     3 BNF: <ELEMENT>     '('   [HERE]  ')'
     4                 |   <NUMBER> BL C, ;BNF
     5 BNF: <PRIMARY>      '-'  <PRIMARY>  ," MINUS "
     6                 |    <ELEMENT> ;BNF
     7 BNF: <FACTOR>      <PRIMARY> '^' <FACTOR>      ," POWER "
     8                 |  <PRIMARY> ;BNF
     9 BNF: <T'>      '*' <FACTOR>     ," * "    <T'>
    10             |  '/' <FACTOR>     ," / "    <T'>
    11             |  ;BNF
    12 BNF: <TERM>     <FACTOR> <T'>       ;BNF
    13 BNF: <E'>      '+' <TERM>    ." + "   <E'>
    14             |  '-' <TERM>    ." - "   <E'>
    15             |  ;BNF

                                                                  Scr#       11
     0  \ BNF Example #3 - code generation                   18 9 88 bjr 21:57
     1  BNF: <EXPRESSION>       <TERM> <E'> ;BNF
     2  ' <EXPRESSION> CFA SWAP             \ fix the recursion in <ELEMENT>
     3
     4  : PARSE    HERE 1 SUCCESS !         <EXPRESSION> <EOL>
     5     CR SUCCESS @ IF HERE OVER - DUP MINUS ALLOT TYPE
     6     ELSE ." Failed" THEN ;
     7
     8
     9
    10
    11
    12
    13
    14
    15

Kragen’s [Wireworld simulator] in Ruby with ruby-processing:

[Wireworld simulator]: http://canonical.org/~kragen/sw/inexorable-misc/wireworld.rb

    # -*- coding: utf-8 -*-
    # An interactive visualization of the Wireworld cellular 
    # automaton. No persistence yet.

    class Wireworld < Processing::App
      def setup
        color_mode RGB, 1.0
        no_stroke
        smooth
        background 0.45

        @cellsize = 10              # pixels
        @nx = width / @cellsize
        @ny = height / @cellsize

        @cells = fresh_cells
        @dirty = []                 # coordinates of dirty cells

        mark_all_cells_dirty
      end

      def draw
        @dirty.each { |item| draw_cell item.first, item.last }
        @dirty = []
        run_wireworld_rule
      end

      def mouse_clicked
        x = mouse_x / @cellsize
        y = mouse_y / @cellsize

        if @cells[x][y] == :empty
          set x, y, :wire
        elsif mouse_button == LEFT
          set x, y, :empty
        else
          set x, y, :electron_head
        end
      end

      def draw_cell(x, y)
        set_color_from @cells[x][y]
        rect(x * @cellsize, y * @cellsize, 
             @cellsize - 2, @cellsize - 2)
      end

      def set_color_from(state)
        case state
        when :empty
          fill 0.5, 0.5, 0.5, 1
        when :wire
          fill 0.75, 0.75, 0.5, 1
        when :electron_head
          fill 1, 1, 1, 1
        when :electron_tail
          fill 0.75, 0.75, 0.75, 1
        end
      end

      def run_wireworld_rule
        old_cells = @cells
        @cells = fresh_cells

        each_coord do |x,y|
          # This could be optimized somewhat by only recalculating
          # the neighbors of dirty cells.
          case old_cells[x][y]
          when :electron_head
            set x, y, :electron_tail
          when :electron_tail
            set x, y, :wire
          when :empty
            # do nothing; fresh_cells are all :empty
          when :wire
            case electron_head_count old_cells, x, y
            when 1, 2
              set x, y, :electron_head
            else
              # Don’t call `set` in this case so as not to mark 
              # the cell dirty for redrawing.
              @cells[x][y] = :wire
            end
          end
        end
      end

      # This could perhaps be optimized somewhat with a sum table.
      def electron_head_count(cells, base_x, base_y)
        count = 0
        ([0, base_x-1].max..[base_x+1, @nx-1].min).each do |x|
          ([0, base_y-1].max..[base_y+1, @ny-1].min).each do |y|
            count += 1 if cells[x][y] == :electron_head
          end
        end
        return count
      end

      def each_coord
        (0..@nx-1).each do |x|
          (0..@ny-1).each do |y|
            yield x, y
          end
        end
      end      

      def mark_all_cells_dirty
        each_coord do |x, y|
          @dirty << [x, y]
        end
      end

      def set(x, y, value)
        @cells[x][y] = value
        @dirty << [x, y]
      end

      def fresh_cells
        Array.new(@nx) { Array.new(@ny, :empty) }
      end
    end

    Wireworld.new(:title => "Wireworld",
                  :width => 1024, :height => 600, 
                  :full_screen => true)

    # Local Variables:
    # compile-command: "./rp5 run wireworld.rb"
    # End:

Some kind of micro-CMCS?  Like a tiny Wiki?

Bernd Paysan’s [one-screeners][], in particular the [object system][].

[one-screeners]: http://www.jwdt.com/~paysan/screenful.html
[object system]: http://www.jwdt.com/~paysan/mini-oof.html

    \ Mini-OOF                                                 12apr98py
    : method ( m v -- m' v ) Create  over , swap cell+ swap
      DOES> ( ... o -- ... ) @ over @ + @ execute ;
    : var ( m v size -- m v' ) Create  over , +
      DOES> ( o -- addr ) @ + ;
    : class ( class -- class methods vars ) dup 2@ ;
    : end-class  ( class methods vars -- )
      Create  here >r , dup , 2 cells ?DO ['] noop , 1 cells +LOOP
      cell+ dup cell+ r> rot @ 2 cells /string move ;
    : defines ( xt class -- ) ' >body @ + ! ;
    : new ( class -- o )  here over @ allot swap over ! ;
    : :: ( class "name" -- ) ' >body @ + @ compile, ;
    Create object  1 cells , 2 cells ,

The Lisp 1.5 metacircular interpreter?

Some interpreters from EOPL?

Some things from IOCCC, if deobfuscated?

Neel Krishnaswami's [90-line compiler][] for the λ-calculus in OCaml:

[90-line compiler]: http://www.reddit.com/r/programming/comments/711ha/llvmbased_miniml_compiler_in_100_lines_of_ocaml/c05eyms

> Sure thing! Here's a compiler for the pure lambda calculus using a cbv evaluation strategy. It compiles to a triple-style pseudo-assembly, which you can easily change to your favorite actual assembly language. The registers I use are:
> 
> * sp -- the stack pointer; points to the topmost occupied element of the stack. The stack grows upwards, so incrementing it yields a new slot.
> * hp -- the heap pointer; points to the next free pointer. Allocation consists of bumping the heap pointer. You aren't getting deallocation in 100 LOC. :-)
> * ep -- the environment register. Hold a pointer to the current environment for lexical variables.
> * ret -- the return pointer. Where the current function should return to.
> * work -- a scratch register
> * newenv -- where we store the environment of a function we're about to call
> * calltgt -- where we store the address of a function we're about to jump to
> 
> The compiler is insanely junky, but hey, it's ninety lines of code. There are exactly two interesting things it does. First, it does closure conversion, and uses `expr` to represent regular asts and `cexpr` for closure converted expressions. Second, it uses a perhaps excessively-slick bit of higher order functional programming to do relocation and backpatching in a purely functional way. Basically, a relocatable piece of code is a function that takes in its start address, and returns a pair consisting of the length of the generated code, and another function which actually produces the code once you give it a table of offsets for the closure addresses.
> Some of the junky things I do is put too much code into the call sequence, rather than into the closure body. Another junky thing is that stack manipulation is incredibly lazy and naive; you could clean it up, shrink the generated code, and probably get rid of a couple of registers. Also, variable references are linear time, since I scan a linked list to find them.
> 
>     type 'a exp =
>       | Var of string
>       | App of 'a exp * 'a exp
>       | Lam of string * 'a 
> 
>     type expr = E of expr exp
>     type cexpr = C of int 
> 
>     let rec lambda_lift e env table =
>       match e with
>       | Var v -> Var v, table
>       | App(e1, e2) ->
>           let e1', table = lambda_lift e1 env table in
>           let e2', table = lambda_lift e2 env table in
>           App(e1', e2'), table
>       | Lam(x, E ebody) ->
>           let ebody', table = lambda_lift ebody (x :: env) table in
>           Lam(x, C(List.length table)), (table @ [x :: env, ebody'])
> 
>     let rec index x = function
>       | [] -> raise Not_found
>       | y :: ys -> if x = y then 0 else 1 + (index x ys)
> 
>     let rec natfold n f init = if n = 0 then init else f (natfold (n-1) f init)
> 
>     (* compile : (string list * cexpr) -> int -> int * (int list -> string list) *)
> 
>     let rec compile' (env, e) start =  
>       match e with
>       | Var x ->
>           let n = index x env in
>           (n + 3,
>            (fun _ -> 
>               ["work := ep\n"] @
>               (natfold n (fun acc -> "work := [work] + 1\n" :: acc) []) @
>               ["sp := sp + 1\n";
>                "[sp] := [work]\n"]))
>       | Lam(x, C id) ->
>           (5,
>            fun locs ->
>              ["sp := sp + 1\n";
>               "[sp] := hp\n";
>               "hp := hp + 2\n";
>               Printf.sprintf "[sp] := %d\n" (List.nth locs id);
>               "[[sp] + 1] := ep\n"])
>       | App(e1, e2) ->
>           let len1, f1 = compile' (env, e1) start in
>           let len2, f2 = compile' (env, e2) (start + len1) in
>           (len1 + len2 + 21,
>            fun locs -> 
>              let code1 = f1 locs in
>              let code2 = f2 locs in
>              (code1 @ code2 @
>               ["work := hp\n";
>                "hp := hp + 2\n";
>                "[work] := [sp]\n";
>                "[[work] + 1] := ep\n";
>                "sp := sp - 1\n";
>                "newenv := [[sp]]\n";
>                "calltgt := [[sp] + 1]\n";
>                "sp := sp - 1\n";
>                "[sp] := ep\n";
>                "sp := sp + 1\n";
>                "[sp] := ret\n";
>                "sp := sp + 1\n";
>                "ep := newenv\n";
>                Printf.sprintf "ret := %d\n" (start + len1 + len2 + 16);
>                "jump calltgt\n";
>                "work := [sp]\n";
>                "sp := sp - 1\n";
>                "ret := [sp]\n";
>                "sp := sp - 1\n";
>                "ep := [sp]\n";
>                "sp := sp - 1\n"]))
> 
>     let rec compile_closures table start =
>       match table with
>       | [] -> ([], [])
>       | pair :: tail ->
>           let (len, code) = compile pair start in
>           let code lst = code lst @ ["jump ret\n"] in
>           let len = len + 1 in
>           let (offsets, codes) = compile_closures tail (start + len) in
>           (start :: offsets, code :: codes)
> 
>     let compile e env =
>       let ce, table = lambda_lift e env [] in
>       let (start, codegen) = compile' (env, ce) 0 in
>       let start = start + 1 in
>       let codegen = (fun offsets -> codegen offsets @ ["halt\n"]) in
>       let (offsets, closuregens) = compile_closures table start in
>       codegen offsets @ (List.concat (List.map (fun f -> f offsets) closuregens))

Some things from [the demo scene][] and size-coding compos?  Maybe
some other graphics hacks?

[the demo scene]: http://canonical.org/~kragen/demo/ "Also check out pouet.net, dude"
