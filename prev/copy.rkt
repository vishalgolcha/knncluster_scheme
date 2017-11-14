#lang racket
;( define ( sum a b)(+ a b))
; c_l create_list just traverses and copies in another null list 
( define (c_l rcvd send fwd bck )
   ( if (= bck 0) send (cons (car rcvd) (c_l ( cdr rcvd) send (+ fwd 1) (- bck 1) ) ) ) 
)
;( file->lines "input.txt" )
;( open-input-file "input.txt" )
; use string->number "1234"

;c_l_num converts string list to numeric
( define (c_l_num rcvd send fwd bck )
   ( if (= bck 0) send (cons (string->number (car rcvd)) (c_l_num ( cdr rcvd) send (+ fwd 1) (- bck 1) ) ) ) 
)

(define reader (cdr (file->lines "t2.in")))
(define fline (car (file->lines "t2.in")))
(define params (c_l_num (string-split fline) '() 0 (length (string-split fline)) ) )
;(define rmfst  (cdr reader ) )

;might need changes 

; use string split

;create numeric list from file
( define (c_l_from_file rcvd send fwd bck )
   ( if (= bck 0) send (cons (c_l_num (string-split(car rcvd)) '() '0 (length (string-split(car rcvd)) )) (c_l_from_file ( cdr rcvd) send (+ fwd 1) (- bck 1) ) ) ) 
)

; works
;enumerated list from file 
( define (enum_file rcvd send fwd bck )
   ( if (= bck 0) send (cons (cons (+ 1  fwd ) (list (c_l_num (string-split(car rcvd)) '() '0 (length (string-split(car rcvd)) )))) (enum_file ( cdr rcvd) send (+ fwd 1) (- bck 1) ) ) ) 
)

(define step1 (enum_file reader '() 0 (length reader)))
; calculate euclidean distance between points 
;( define ( sub_pwr x y)  ( * (+ x y) (+ x y) )  )  

( define (dist a b c)     
         ( if (= (length a) '0 ) c ( dist (cdr a) (cdr b)  ( + ( ( lambda (x y)  ( * (- x y ) (- x y ) ) )  (car a)(car b)) c ) ) )      
)
;for better readability
( define ( fout x) (file->lines x))  

;check for cile length -1 once again 
( define (line_extr x) ( c_l_from_file (fout x) '() 0 (length (fout x)) ) ) 
( define (rw_len x) (length  x))
( define (co_len x) (length ( car x ) ) ) 

;let's calculate the similarity vector for a particular vector inp_v 
;invp_v is the input vector , pos_v is the position of the vector in the rows we have numbered

; pass dc_len in bck
;flist param would be input.txt extracted here ; also check if you read the file in reverse earlier as well ! so this rev wont be reqd

( define (sim_vect inp_v pos_v flist fwd bck ret_v)  
     (  if (= bck '0) (reverse ret_v) ;return vector accumulated ; check on reverse on other parts of the code as well     
        ;iterator else;
            (
               if ( = pos_v fwd ) (sim_vect inp_v pos_v (cdr flist) (+ fwd '1)(- bck '1) (cons ( list (+ fwd '1) +inf.0) ret_v )) 
               ; pos not else
                   (sim_vect inp_v pos_v (cdr flist) (+ fwd '1)(- bck '1) (cons ( list (+ fwd '1) ( sqrt (dist inp_v (car flist) '0 ))) ret_v))
               ; pos not else end     
            )
         ;iterator else end

      )  
)

; now accumulating similarity matrix
; bck should be row size here
; need a seperate copy of flist here 
( define (sim_mat flist flist2 fwd bck ret_m)
   ( if( = bck '0 ) (reverse ret_m)
       (sim_mat (cdr flist) flist2 (+ fwd '1)(- bck '1) (cons (sim_vect ( car flist ) fwd flist2 '0 (rw_len flist2) '()) ret_m))    
   )
)


;try query (sim_vect (list '1.2 '1.3 '2) '1 (list(list'0 '1 '0) (list '1 '1 '2) (list '1 '3 '2) ) '0 '3'())
         ;'((1 2.3515952032609695) (2 +inf.0) (3 1.7117242768623688))

;(sim_mat (line_extr "input.txt") (line_extr "input.txt") '0 (rw_len (line_extr "input.txt") )  '())

;here as well need to remove the first line 
(define  step2inp (cdr (line_extr "t2.in" )) )  


(define matrix_out (sim_mat step2inp step2inp '0 (rw_len step2inp )  '()) )

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define step2 (modify_precision matrix_out) )
; now query (matrix_out "input.txt")

; k extracted from first line
;(define  ( mat_sorter x) ( sort (matrix_out x) < ) )

;sort based on second value in list implemented below 
;( sort(list (list '1 '2)  (list '12 '3) ) ( lambda (x y)  ( < (car ( cdr x ) ) (car (cdr y ) ) ) ) ) 
(define ( sec x )  (car(cdr x)) )
(define ( fir x )  (car x))
(define ( sort_sec  a)  ( sort a ( lambda (x y)  ( if ( = (sec x) (sec y) ) (< (fir x) (fir y) ) (< (sec x) (sec y)))) ) )
(define ( sorted_mat rcv send bck ) ( if (= bck '0) (reverse send ) (sorted_mat (cdr rcv) (cons (sort_sec (car rcv)) send ) (- bck 1) ) )  )

;query (sorted_mat (matrix_out "input.txt") '() (length (matrix_out "input.txt") ))
(define dist_ord  (sorted_mat step2  '() (length step2  )))

; g_n_k referes to give nearest k neighbors 
(define (g_n_k k lst send)( if (= k '0) ( reverse send)(g_n_k (- k 1) (cdr lst) (cons (car lst) send))))
;knearest nerighbors

; give dist_ord matrix to this func 
(define (knn_mat k rows mat send ) (if (= rows '0)(reverse send)(knn_mat k (- rows '1)(cdr mat)(cons (g_n_k k (car mat) '()) send))) )
; knn matrix generated
; query (knn_mat '5 (length (dist_ord "input.txt")) (dist_ord "input.txt") '() )
(define  (knn_show k x) (knn_mat k (length x) x '()) )
;q ( knn_show 5 (dist_ord "input.txt") )

;extracts first element from the list 
(define (f_extr rcv bck send) ( if (= bck '1) (reverse (cons (car (car rcv)) send)) (f_extr (cdr rcv)(- bck 1) (cons (car (car rcv)) send)))) 

;matrix with index vectors 
(define (f_mat rcv bck send ) ( if (= bck '1) (reverse (cons (f_extr (car rcv) (length (car rcv)) '()) send)) (f_mat (cdr rcv)(- bck 1) (cons (f_extr (car rcv) (length (car rcv)) '()) send))))

;( f_mat (knn_show 5 (dist_ord "input.txt")) (length (knn_show 5 (dist_ord "input.txt")) ) '())
;function with just input file given

(define index_rep  ( f_mat (knn_show (third params) dist_ord) (length (knn_show (third params) dist_ord ) ) '()))

;sorted index rep for using binary search rcvs matrix from index_rep k x call 
(define (s_i_rep rcv send bck) (if (= bck '1)( reverse (cons (sort (car rcv) <) send )) (s_i_rep (cdr rcv) (cons (sort (car rcv) <) send) (- bck  1)) )) 
; use as (s_i_rep (index_rep 5 ) '() (length (index_rep 5 )))

(define step3  (s_i_rep index_rep '() (length index_rep)))



; code for finding intersection

(define (in-list? val ls)
  (if (null? ls) #f 
    ;else begins 
    (if 
        (= (car ls) val) #t
        (in-list? val (cdr ls))
    )
  )
)

(define (intersect ls1 ls2)
  (if (null? ls1)'()
      ;else begins
      (if (in-list? (car ls1) ls2) 
        (cons (car ls1) (intersect (cdr ls1) ls2))
        (intersect (cdr ls1) ls2)
    )
  )
)

; code for intersection ends
(define (delete item list) (filter (lambda (x) (not (equal? x item))) list))

(define (iter lst ind )( if(= ind '1)(car lst)(iter (cdr lst)(- ind '1)))) 

;checks if element from is in to's lst
(define ( sort_sec_r  a)  ( sort a ( lambda (x y)  ( if ( = (sec x) (sec y) ) (< (fir x) (fir y) ) (> (sec x) (sec y)))) ) )

(define (send_edge to from lst) (if (in-list? from lst) (list to (length(intersect (iter step3 from) (iter step3 to))) ) (list '-1)))

;pushes if string not null
(define (push lst send from to) ( if (null? (send_edge to from lst) ) (send) (cons (send_edge to from lst) send)))

; this receives  from's list and accumulates send vector for from 
(define (edge_list from lst send bck)
   (if ( = bck 1) (push (iter step3 (car lst)) send from (car lst))
       ;else
       (edge_list from (cdr lst) (push (iter step3 (car lst)) send from (car lst)) (- bck 1))
       ))


(define (generator fwd) (edge_list fwd (iter step3 fwd) '() (length (iter step3 fwd)) ) )
;fwd should start with 1 here 
(define (modif fwd )(sort_sec_r (delete (list'-1) (generator fwd))))
(define (accum fwd send) ( if(= fwd (+ (length step3) '1)) (reverse send)
                                  ;else
                              (accum (+ fwd '1)
                                     (cons (modif fwd) send) ) ))
; sends list to above function to iterate 
;(define (car_send fwd bck send) ( if (= bck 1) (reverse((cons (  send))) (car_send) ) )

(define step4 (accum '1 '()))

(define (step5_filt y) ( filter (lambda (x) (>= (sec x)  (fourth params))) y)) 

;(define (laccum lst send fwd) (if (= fwd (+ (length lst) '1 )) (reverse send)
;                                  (laccum lst (cons (length (iter lst fwd)) send) (+ fwd '1)))))

(define (pre_step5 frw send) (if(= frw (+ 1 (length step4))) (reverse send)
                           ;else
                           (pre_step5 (+ frw 1) (cons (length (step5_filt (iter step4 frw))) send))
                           ))
(define step5 (pre_step5 1 '()) )

(define (pre_step6 frw send) ( if(= frw (+ 1 (length step5)))(reverse send) (
                                                                             if(>= (iter step5 frw) (fifth params))
                                                                             (pre_step6 (+ 1 frw ) (cons frw send))
                                                                             (pre_step6 (+ 1 frw ) send))  ))
(define step6 (pre_step6 1 '()))

(define (pre_step8 frw send) ( if(= frw (+ 1 (length step5)))(reverse send) (
                                                                             if( and (< (iter step5 frw) (fifth params))
                                                                                     (equal? (iter step5 frw) '0) )
                                                                             (pre_step8 (+ 1 frw ) (cons frw send))
                                                                             (pre_step8 (+ 1 frw ) send))  ))
(define step8 (pre_step8 1 '()) )
(define (pre_step9 frw send) ( if(= frw (+ 1 (length step5)))(reverse send) (
                                                                             if( and (< (iter step5 frw) (fifth params))
                                                                                     (> (iter step5 frw) '0) )
                                                                             (pre_step9 (+ 1 frw ) (cons frw send))
                                                                             (pre_step9 (+ 1 frw ) send))  ))

(define step9 (pre_step9 1 '()) )
