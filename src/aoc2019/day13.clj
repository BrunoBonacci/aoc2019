(ns aoc2019.day13
  (:require [aoc2019.intcode-machine :refer :all]
            [clojure.string :as str]
            [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [where.core :refer [where]]
            [aoc2019.util :refer :all]))



;;
;; --- Day 13: Care Package ---
;;
;; As you ponder the solitude of space and the ever-increasing
;; three-hour roundtrip for messages between you and Earth, you notice
;; that the Space Mail Indicator Light is blinking. To help keep you
;; sane, the Elves have sent you a care package.
;;
;; It's a new game for the ship's arcade cabinet! Unfortunately, the
;; arcade is all the way on the other end of the ship. Surely, it
;; won't be hard to build your own - the care package even comes with
;; schematics.
;;
;; The arcade cabinet runs Intcode software like the game the Elves
;; sent (your puzzle input). It has a primitive screen capable of
;; drawing square tiles on a grid. The software draws tiles to the
;; screen with output instructions: every three output instructions
;; specify the x position (distance from the left), y position
;; (distance from the top), and tile id. The tile id is interpreted as
;; follows:
;;
;;   - 0 is an empty tile. No game object appears in this tile.
;;   - 1 is a wall tile. Walls are indestructible barriers.
;;   - 2 is a block tile. Blocks can be broken by the ball.
;;   - 3 is a horizontal paddle tile. The paddle is indestructible.
;;   - 4 is a ball tile. The ball moves diagonally and bounces off objects.
;;
;; For example, a sequence of output values like 1,2,3,6,5,4 would
;; draw a horizontal paddle tile (1 tile from the left and 2 tiles
;; from the top) and a ball tile (6 tiles from the left and 5 tiles
;; from the top).
;;
;; Start the game. How many block tiles are on the screen when the
;; game exits?
;;


(def prog (parse (slurp (io/resource "day13-input.txt"))))



(fact
 "solution: How many block tiles are on the screen when the game exits?"

 (->> (machine-eval prog)
    :output
    (partition-all 3)
    (filter (where last :is? 2))
    count)
 => 270
 )



;;


(defn screen-update
  [{:keys [screen output] :as m}]
  (let [screen' (->> (partition-all 3 output)
                   (map (fn [[x y v]] [[x y] v]))
                   (into screen))]
    (-> m
       (assoc :screen screen')
       (assoc :output []))))



(defn score
  [screen]
  (get screen [-1 0] 0))



(defn draw
  [screen]
  (let [mmax (fn ([] 0) ([& vs] (apply max vs)))
        _ (def xs screen)
        width  (->> screen (map ffirst) (reduce mmax) inc)
        height (->> screen (map (comp second first)) (reduce mmax) inc)]
    (->>
     (for [y (range height)
         x (range width)]
       (case (get screen [x y] 0)
         0 " "
         1 "#"
         2 "%"
         3 "="
         4 "*"))
     (partition width)
     (map (partial apply str))
     (cons (format "[score: %d]" (score screen)))
     (str/join "\n")
     )))



(defn print-screen
  [{:keys [screen]}]
  (print
   (str "\033[2J" (draw screen) \newline))
  (flush))



(defn paddle-move
  [{:keys [screen input] :as m}]
  (let [[[bx]] (first (filter (where second = 4) screen))
        [[px]] (first (filter (where second = 3) screen))
        direction (sign (- bx px))]
    (update m :input conj direction)))



(defn game-step
  [machine]
  (->> (step-through-until-breakpoint machine before-input?)
     (screen-update)
     (paddle-move)))



(defn more-blocks?
  [{:keys [screen]}]
  (->> screen
     (filter (where second = 2))
     (count)
     (< 0)))



(defn run-game
  [prog & {:keys [print-screen?] :or {print-screen? true}}]
  (let [display (if print-screen? #(doto % print-screen) identity)]
    (->> (iterate game-step (game-step {:state prog :screen {}}))
       (map display)
       (take-until (complement running?))
       (last)
       :screen
       score)))



(fact
 "solution What is your score after the last block is broken? "
 (run-game (assoc prog 0 2) :print-screen? false)
 => 12535

 )



;;
