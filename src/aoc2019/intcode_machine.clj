(ns aoc2019.intcode-machine
  (:require [clojure.string :as str]
            [midje.sweet :refer :all]))


(defn opcode
  [code]
  (let [op    (rem  code 100)
        code  (quot code 100)
        pax   (rem  code  10)
        code  (quot code  10)
        pbx   (rem  code  10)
        code  (quot code  10)
        pcx   code]
    {:op op :pax pax :pbx pbx :pcx pcx}))


(fact
 "opcode parsing"

 (opcode 1)   => {:op 1,   :pax 0, :pbx 0, :pcx 0}
 (opcode 2)   => {:op 2,   :pax 0, :pbx 0, :pcx 0}
 (opcode 99)  => {:op 99,  :pax 0, :pbx 0, :pcx 0}
 (opcode 1002) => {:op 2,  :pax 0, :pbx 1, :pcx 0}
 (opcode 11101) => {:op 1, :pax 1, :pbx 1, :pcx 1}

 )


(defn $get
  [state addr mode]
  (if (= 1 mode)
    addr
    (get state addr)))



(defn $set
  [state addr mode value]
  (if (= 1 mode)
    (assoc state (get state addr) value)
    (assoc state addr value)))



(defn- eval-dispatch
  [{:keys [state pp]}]
  (if (number? pp)
    (-> (get state pp) (rem 100))
    :default))



(defmulti eval-instruction eval-dispatch)



(defn instruction
  [{:keys [state pp input output] :as m} expected-args]
  (if (>= (count state) (+ expected-args pp))
    (-> (subvec state pp (+ pp expected-args))
       (update 0 opcode))
    (throw (ex-info "missing instruction arguments"
                    {:state state
                     :pp pp
                     :expected-args expected-args }))))



(defmethod eval-instruction :default
  [{:keys [state pp input output] :as m}]
  (assoc m :pp nil))



(defmethod eval-instruction 1 ;; sum
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx cx] (instruction m 4)]
    (-> m
       (update :pp + 4)
       (update :state $set cx pcx (+' ($get state ax pax) ($get state bx pbx))))))



(fact
 "verifying sum"

 (eval-instruction {:state [1 2 3 0 99] :pp 0}) => (contains {:state [3 2 3 0 99] :pp 4})
 (eval-instruction {:state [1101 2 3 0 99] :pp 0}) => (contains {:state [5 2 3 0 99] :pp 4})
 )



(defmethod eval-instruction 2 ;; multiplication
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx cx] (instruction m 4)]
    (-> m
       (update :pp + 4)
       (update :state $set cx pcx (*' ($get state ax pax) ($get state bx pbx))))))


(fact
 "verifying multiplication"

 (eval-instruction {:state [2 2 3 0 99] :pp 0}) => (contains {:state [0 2 3 0 99] :pp 4})
 (eval-instruction {:state [1102 2 3 0 99] :pp 0}) => (contains {:state [6 2 3 0 99] :pp 4})
 )


;; read-input
(defmethod eval-instruction 3
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax]} ax] (instruction m 2)]
    (-> m
       (update :pp + 2)
       (update :state $set ax pax (first input))
       (update :input rest))))


(fact
 "verifying input handling"

 (eval-instruction {:state [3 1 99] :pp 0 :input [9]}) => (contains {:state [3 9 99] :pp 2 :input []})
 (eval-instruction {:state [103 3 99 0] :pp 0 :input [9]}) => (contains {:state [9 3 99 0] :pp 2 :input []})
 )

;; output
(defmethod eval-instruction 4
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax]} ax] (instruction m 2)]
    (-> m
       (update :pp + 2)
       (update :output conj ($get state ax pax)))))


(comment

  ;; jump-if-true
  (defmethod eval-instruction 5
    [{:keys [state pp input output] :as m}]
    (let [[{:keys [op p1 p2]} a b] (instruction m 3)
          condition (not= 0 a)]
      [state (if condition b (+ 3 pp))]))


  ;; jump-if-false
  (defmethod eval-instruction 6
    [{:keys [state pp input output] :as m}]
    (let [[{:keys [op p1 p2]} a b] (instruction m 3)
          condition (= 0 a)]
      [state (if condition b (+ 3 pp))]))


  ;; less-than
  (defmethod eval-instruction 7
    [{:keys [state pp input output] :as m}]
    (let [[{:keys [op p1 p2 p3]} a b c] (instruction m 4)
          condition (< ($get state a p1) ($get state b p1))]
      [($set state c p3 (if condition 1 0))
       (+ 4 pp)]))


  ;; equals
  (defmethod eval-instruction 8
    [{:keys [state pp input output] :as m}]
    (let [[{:keys [op p1 p2 p3]} a b c] (instruction m 4)
          condition (= ($get state a p1) ($get state b p1))]
      [($set state c p3 (if condition 1 0))
       (+ 4 pp)])))



(defmethod eval-instruction 99
  [{:keys [state pp input output] :as m}]
  [state :terminated])



(defn parse
  [p]
  (->> (str/split p #",")
     (mapv read-string)))


(fact
 "parse instruction set"

 (parse "1,0,0,0,99") => [1 0 0 0 99]
 (parse "1,0,-1,0,99") => [1 0 -1 0 99]
 )


(defn ended?
  [{:keys [pp]}]
  (not (nil? pp)))



(defn machine-eval
  ([p]
   (->> (iterate eval-instruction {:state p :pp 0})
      (take-while ended?)
      last))
  ([p input]
   (->> (iterate eval-instruction {:state p :pp 0 :input input :output []})
      (take-while ended?)
      last)))



(fact
 "micro machine evaluates programs correctly"

 (:state (machine-eval (parse "1,9,10,3,2,3,11,0,99,30,40,50")))
 => [3500,9,10,70, 2,3,11,0, 99, 30,40,50]

 (:state (machine-eval (parse "1,0,0,0,99")))
 => [2,0,0,0,99]

 (:state (machine-eval (parse "2,3,0,3,99")))
 => [2,3,0,6,99]

 (:state (machine-eval (parse "2,4,4,5,99,0")))
 => [2,4,4,5,99,9801]

 (:state (machine-eval (parse "1,1,1,4,99,5,6,0,99")))
 => [30,1,1,4,2,5,6,0,99]

 (:state (machine-eval (parse "1002,4,3,4,33")))
 => [1002 4 3 4 99]
 )



(fact
 "day 2 challenge"

 (let [program
       "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,9,19,23,2,23,10,27,1,27,5,31,1,31,6,35,1,6,35,39,2,39,13,43,1,9,43,47,2,9,47,51,1,51,6,55,2,55,10,59,1,59,5,63,2,10,63,67,2,9,67,71,1,71,5,75,2,10,75,79,1,79,6,83,2,10,83,87,1,5,87,91,2,9,91,95,1,95,5,99,1,99,2,103,1,103,13,0,99,2,14,0,0"]



   (->> program
      (parse)
      (machine-eval)
      :state
      first))

 => 3931283
 )



#_(fact
 "testing compartors positional"

 (:state (machine-eval (parse "8,5,6,0,99,9,9")))
 => [1 5 6 0 99 9 9]


 (first (machine-eval (parse "8,5,6,0,99,8,9")))
 => [0 5 6 0 99 8 9]


 (first (machine-eval (parse "10008,3,2,1,99")))
 => [10008 3 2 0 99]


 (first (machine-eval (parse "7,5,6,0,99,8,9")))
 => [1 5 6 0 99 8 9]


 (first (machine-eval (parse "7,5,6,0,99,9,9")))
 => [0 5 6 0 99 9 9]


 (first (machine-eval (parse "10007,2,3,1,99")))
 => [10007 2 0 1 99]

 )
