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


(defmethod eval-instruction 3 ;; read-input
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax]} ax] (instruction m 2)]
    (-> m
       (update :pp + 2)
       (update :state $set ax pax (or (first input) (throw (ex-info "Reached End Of Inputs (EOI)." m))))
       (update :input rest))))


(fact
 "verifying input handling"

 (eval-instruction {:state [3 1 99] :pp 0 :input [9]}) => (contains {:state [3 9 99] :pp 2 :input []})
 (eval-instruction {:state [103 3 99 0] :pp 0 :input [9]}) => (contains {:state [9 3 99 0] :pp 2 :input []})
 )


(defmethod eval-instruction 4 ;; output
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax]} ax] (instruction m 2)]
    (-> m
       (update :pp + 2)
       (update :output conj ($get state ax pax)))))


(fact
 "verifying output handling"

 (eval-instruction {:state [4 2 99] :pp 0 :output []}) => (contains {:state [4 2 99] :pp 2 :output [99]})
 (eval-instruction {:state [104 2 99] :pp 0 :output []}) => (contains {:state [104 2 99] :pp 2 :output [2]})
 )




(defmethod eval-instruction 5 ;; jump-if-true
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx] (instruction m 3)
        condition (not= 0 ($get state ax pax))]
    (if condition
      (assoc  m :pp ($get state bx pbx))
      (update m :pp + 3))))


(fact
 "verifying jump-if-true handling"

 (eval-instruction {:state [5 4 4 99 5 99] :pp 0})   => (contains {:state [5 4 4 99 5 99], :pp 5})
 (eval-instruction {:state [5 4 4 99 0] :pp 0})      => (contains {:state [5 4 4 99 0], :pp 3})
 (eval-instruction {:state [1105 1 4 99 99] :pp 0 }) => (contains {:state [1105 1 4 99 99] :pp 4 })
 (eval-instruction {:state [1105 0 4 99 99] :pp 0 }) => (contains {:state [1105 0 4 99 99] :pp 3 })
 )


(defmethod eval-instruction 6 ;; jump-if-false
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx] (instruction m 3)
        condition (= 0 ($get state ax pax))]
    (if condition
      (assoc  m :pp ($get state bx pbx))
      (update m :pp + 3))))


(fact
 "verifying jump-if-false handling"

 (eval-instruction {:state [6 4 5 99 0 6 99] :pp 0}) => (contains {:state [6 4 5 99 0 6 99], :pp 6})
 (eval-instruction {:state [6 4 4 99 1] :pp 0})      => (contains {:state [6 4 4 99 1], :pp 3})
 (eval-instruction {:state [1106 0 4 99 99] :pp 0 }) => (contains {:state [1106 0 4 99 99], :pp 4})
 (eval-instruction {:state [1106 1 4 99 99] :pp 0 }) => (contains {:state [1106 1 4 99 99], :pp 3})
 )


(defmethod eval-instruction 7 ;; less-than
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx cx] (instruction m 4)
        condition (< ($get state ax pax) ($get state bx pbx))]
    (-> m
       (update :pp + 4)
       (update :state $set cx pcx (if condition 1 0)))))


(fact
 "verifying less-than handling"

 (eval-instruction {:state [7 5 6 7 99 1 2 -1] :pp 0})    => (contains {:state [7 5 6 7 99 1 2 1], :pp 4})
 (eval-instruction {:state [7 6 5 7 99 1 2 -1] :pp 0})    => (contains {:state [7 6 5 7 99 1 2 0], :pp 4})
 (eval-instruction {:state [1107 5 6 7 99 1 2 -1] :pp 0}) => (contains {:state [1107 5 6 7 99 1 2 1], :pp 4})
 (eval-instruction {:state [1107 6 5 7 99 1 2 -1] :pp 0}) => (contains {:state [1107 6 5 7 99 1 2 0], :pp 4})
 )




(defmethod eval-instruction 8 ;; equals
  [{:keys [state pp input output] :as m}]
  (let [[{:keys [op pax pbx pcx]} ax bx cx] (instruction m 4)
        condition (= ($get state ax pax) ($get state bx pbx))]
    (-> m
       (update :pp + 4)
       (update :state $set cx pcx (if condition 1 0)))))


(fact
 "verifying equal handling"

 (eval-instruction {:state [8 5 6 7 99 1 1 -1] :pp 0})    => (contains {:state [8 5 6 7 99 1 1 1], :pp 4})
 (eval-instruction {:state [8 6 5 7 99 1 2 -1] :pp 0})    => (contains {:state [8 6 5 7 99 1 2 0], :pp 4})
 (eval-instruction {:state [1108 5 5 7 99 1 2 -1] :pp 0}) => (contains {:state [1108 5 5 7 99 1 2 1], :pp 4})
 (eval-instruction {:state [1108 6 5 7 99 1 2 -1] :pp 0}) => (contains {:state [1108 6 5 7 99 1 2 0], :pp 4})
 )



(defmethod eval-instruction 99
  [{:keys [state pp input output] :as m}]
  (assoc m :pp :terminated))



(defn parse
  [p]
  (->> (str/split p #",")
     (mapv read-string)))


(fact
 "parse instruction set"

 (parse "1,0,0,0,99") => [1 0 0 0 99]
 (parse "1,0,-1,0,99") => [1 0 -1 0 99]
 )


(defn running?
  [{:keys [pp]}]
  (not (nil? pp)))



(defn machine-eval
  ([p]
   (->> (iterate eval-instruction {:state p :pp 0 :input [] :output []})
      (take-while running?)
      last))
  ([p input]
   (->> (iterate eval-instruction {:state p :pp 0 :input input :output []})
      (take-while running?)
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



(fact
 "testing compartors positional"

 (:state (machine-eval (parse "8,5,6,0,99,9,9")))
 => [1 5 6 0 99 9 9]


 (:state (machine-eval (parse "8,5,6,0,99,8,9")))
 => [0 5 6 0 99 8 9]


 (:state (machine-eval (parse "10008,3,2,1,99")))
 => [10008 3 2 0 99]


 (:state (machine-eval (parse "7,5,6,0,99,8,9")))
 => [1 5 6 0 99 8 9]


 (:state (machine-eval (parse "7,5,6,0,99,9,9")))
 => [0 5 6 0 99 9 9]


 (:state (machine-eval (parse "10007,2,3,1,99")))
 => [10007 2 0 1 99]

 )


(fact
 "test jumps from day 05"

 ;; input equal to 8 (position mode)
 (:output (machine-eval (parse "3,9,8,9,10,9,4,9,99,-1,8") [8])) => [1]
 (:output (machine-eval (parse "3,9,8,9,10,9,4,9,99,-1,8") [7])) => [0]

 ;; input less than 8 (position mode)
 (:output (machine-eval (parse "3,9,7,9,10,9,4,9,99,-1,8") [8])) => [0]
 (:output (machine-eval (parse "3,9,7,9,10,9,4,9,99,-1,8") [7])) => [1]


 ;; input equal to 8 (immediate mode)
 (:output (machine-eval (parse "3,3,1108,-1,8,3,4,3,99") [8])) => [1]
 (:output (machine-eval (parse "3,3,1108,-1,8,3,4,3,99") [7])) => [0]

 ;; input less than 8 (immediate mode)
 (:output (machine-eval (parse "3,3,1107,-1,8,3,4,3,99") [8])) => [0]
 (:output (machine-eval (parse "3,3,1107,-1,8,3,4,3,99") [7])) => [1]



 ;; whether the input is zero or non zero
 (:output (machine-eval (parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") [0])) => [0]
 (:output (machine-eval (parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") [8])) => [1]
 (:output (machine-eval (parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") [0])) => [0]
 (:output (machine-eval (parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") [8])) => [1]

 )
