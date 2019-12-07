(ns aoc2019.intcode-machine
  (:require [clojure.string :as str]
            [midje.sweet :refer :all]))


(defn opcode
  [code]
  (let [op   (rem  code 100)
        code (quot code 100)
        p1   (rem  code  10)
        code (quot code 10)
        p2   (rem  code  10)
        code (quot code 10)
        p3   code]
    {:op op :p1 p1 :p2 p2 :p3 p3}))


(fact
 "opcode parsing"

 (opcode 1)   => {:op 1, :p1 0, :p2 0, :p3 0}
 (opcode 2)   => {:op 2, :p1 0, :p2 0, :p3 0}
 (opcode 99)  => {:op 99, :p1 0, :p2 0, :p3 0}

 (opcode 1002) => {:op 2, :p1 0, :p2 1, :p3 0}
 (opcode 11101) => {:op 1, :p1 1, :p2 1, :p3 1}

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
  [[state pp]]
  (if (number? pp)
    (->> (get state pp) opcode :op)
    :default))



(defmulti eval-instruction eval-dispatch)



(defn instruction [[state pp] expected-args]
  (if (>= (count state) (+ expected-args pp))
    (-> (subvec state pp (+ pp expected-args))
       (update 0 opcode))
    (throw (ex-info "missing instruction arguments"
                    {:state state
                     :pp pp
                     :expected-args expected-args }))))



(defmethod eval-instruction :default
  [[state pp]]
  [state nil])


;; sum
(defmethod eval-instruction 1
  [[state pp :as arg]]
  (let [[{:keys [op p1 p2 p3]} a b c] (instruction arg 4)]
    [(->>
      (+' ($get state a p1) ($get state b p2))
      ($set state c p3))
     (+ 4 pp)]))


;; multiplication
(defmethod eval-instruction 2
  [[state pp :as arg]]
  (let [[{:keys [op p1 p2 p3]} a b c] (instruction arg 4)]
    [(->>
      (*' ($get state a p1) ($get state b p2))
      ($set state c p3))
     (+ 4 pp)]))


;; read-input
(defmethod eval-instruction 3
  [[state pp :as arg]]
  (let [[{:keys [op p1]} a] (instruction arg 2)]
    (let [v (read-string (read-line))]
      (prn " IN:" v)
      [($set state a p1 v) (+ 2 pp)])))


;; output
(defmethod eval-instruction 4
  [[state pp :as arg]]
  (let [[{:keys [op p1]} a] (instruction arg 2)]
    (prn "OUT:" ($get state a p1))
    [state (+ 2 pp)]))



(defmethod eval-instruction 99
  [[state pp :as arg]]
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
  [[_ p]]
  (not (nil? p)))



(defn machine-eval
  [p]
  (->> (iterate eval-instruction [p 0])
     (take-while ended?)
     last))



(fact
 "micro machine evaluates programs correctly"

 (first (machine-eval (parse "1,9,10,3,2,3,11,0,99,30,40,50")))
 => [3500,9,10,70, 2,3,11,0, 99, 30,40,50]

 (first (machine-eval (parse "1,0,0,0,99")))
 => [2,0,0,0,99]

 (first (machine-eval (parse "2,3,0,3,99")))
 => [2,3,0,6,99]

 (first (machine-eval (parse "2,4,4,5,99,0")))
 => [2,4,4,5,99,9801]

 (first (machine-eval (parse "1,1,1,4,99,5,6,0,99")))
 => [30,1,1,4,2,5,6,0,99]

 (first (machine-eval (parse "1002,4,3,4,33")))
 => [1002 4 3 4 99]
 )



(fact
 "day 2 challenge"

 (let [program
       "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,9,19,23,2,23,10,27,1,27,5,31,1,31,6,35,1,6,35,39,2,39,13,43,1,9,43,47,2,9,47,51,1,51,6,55,2,55,10,59,1,59,5,63,2,10,63,67,2,9,67,71,1,71,5,75,2,10,75,79,1,79,6,83,2,10,83,87,1,5,87,91,2,9,91,95,1,95,5,99,1,99,2,103,1,103,13,0,99,2,14,0,0"]



   (->> program
      (parse)
      (machine-eval)
      ffirst))

 => 3931283
 )
