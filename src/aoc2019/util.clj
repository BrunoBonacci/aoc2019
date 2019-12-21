(ns aoc2019.util)

(defn sign
  [n]
  (cond
    (= 0 n) 0
    (pos? n) +1
    (neg? n) -1))



(defn take-until [p s]
  (transduce (halt-when p (fn [r h] (conj r h))) conj [] s))
