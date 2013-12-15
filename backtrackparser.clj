(defn trampoline-condition [cc-name statements]
  (list 'fn []
        (if (= (first statements) :else)
          (second statements)
          (list 'let [cc-name (trampoline-condition cc-name (rest (rest statements)))]
                (list 'if
                      (first statements)
                      (list 'fn [] (second statements))
                      cc-name)))))

(defmacro tcond [cc-name & statements]
  (list 'trampoline
        (list 'fn []
              (trampoline-condition cc-name statements))))

(defmacro ccond [cc-name & statements]
  (list 'fn []
        (trampoline-condition cc-name statements)))

(defmacro caar [ls]
  `(first (first ~ls)))

(defn st-equals
  "stack top equals ls "
  [stack ls]
  (loop [stack stack ls (reverse ls)]
    (cond
     (empty? ls)
     ,true
     (= (caar stack) (first ls))
     ,(recur (rest stack) (rest ls))
     :else
     false)))

(defn parser
  "backtrack parser"
  [stack-trace tokens]
  (defn lecur [stack tokens cc]
    (if stack-trace (println (str stack)))
    (ccond
     c-cont
     (empty? tokens)
     ,(if (= 2 (count stack))
        (second stack)
        (do
          (if stack-trace (println "back track !!"))
          cc))
     (st-equals stack [:num])
     ,(lecur (cons [:F (first stack)] (drop 1 stack)) tokens c-cont)
     (st-equals stack [:F])
     ,(lecur (cons [:T (first stack)] (drop 1 stack)) tokens c-cont)
     (st-equals stack [:T])
     ,(lecur (cons [:E (first stack)] (drop 1 stack)) tokens c-cont)
     (st-equals stack [:F :* :T])
     ,(lecur (cons [:T :* (first stack) (nth stack 2)] (drop 3 stack)) tokens c-cont)
     (st-equals stack [:T :+ :E])
     ,(lecur (cons [:E :+ (first stack) (nth stack 2)] (drop 3 stack)) tokens c-cont)
     :else
     (lecur (cons (first tokens) stack) (rest tokens) cc)))
  (trampoline #(lecur [] tokens (fn[] "not-parsable"))))

;;the grammer of the parser above
;; E -> T '+' E
;; T -> F '*' T
;; E -> T
;; T -> F
;; F -> num

(defn parse [tokens]
  (parser false tokens))

(defn parse-with-stack-trace [tokens]
  (parser true tokens))

;;;usage
;;parse without stack trace
;(parse [[:num] [:+] [:num] [:*] [:num] [:$]])
;;parse with stack trace
;(parse-with-stack-trace [[:num] [:+] [:num] [:*] [:num] [:$]])

;;;example
;;user=> (parse [[:num] [:+] [:num] [:*] [:num] [:$]])
;;[:E :+ [:E [:T :* [:T [:F [:num]]] [:F [:num]]]] [:T [:F [:num]]]]
