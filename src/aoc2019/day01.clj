(ns aoc2019.day01
  (:require [midje.sweet :refer :all]))

;; --- Day 1: The Tyranny of the Rocket Equation ---
;;
;; Santa has become stranded at the edge of the Solar System while
;; delivering presents to other planets! To accurately calculate his
;; position in space, safely align his warp drive, and return to Earth
;; in time to save Christmas, he needs you to bring him measurements
;; from fifty stars.
;;
;; Collect stars by solving puzzles. Two puzzles will be made
;; available on each day in the Advent calendar; the second puzzle is
;; unlocked when you complete the first. Each puzzle grants one
;; star. Good luck!
;;
;; The Elves quickly load you into a spacecraft and prepare to launch.
;;
;; At the first Go / No Go poll, every Elf is Go until the Fuel
;; Counter-Upper. They haven't determined the amount of fuel required
;; yet.
;;
;; Fuel required to launch a given module is based on its
;; mass. Specifically, to find the fuel required for a module, take
;; its mass, divide by three, round down, and subtract 2.
;;
;; For example:
;;
;; For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
;; For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
;; For a mass of 1969, the fuel required is 654.
;; For a mass of 100756, the fuel required is 33583.
;;
;; The Fuel Counter-Upper needs to know the total fuel requirement. To
;; find it, individually calculate the fuel needed for the mass of
;; each module (your puzzle input), then add together all the fuel
;; values.
;;
;; What is the sum of the fuel requirements for all of the modules on your spacecraft?
;;


(defn fuel-required
  [mass]
  (- (long (/ mass 3)) 2))


(facts
 "fuels calculation"

 (fuel-required 12) => 2
 (fuel-required 14) => 2
 (fuel-required 1969) => 654
 (fuel-required 100756) => 33583
 )


(def module-masses
  [58444 100562 133484 67910 58372 104607 108786 137410 62910 76115 64142 59324 54327 92864 94120 63931 128696 111758 65698 54930 116136 127111 133914 52992 90364 107637 62118 147901 62347 53614 140690 115587 66148 95729 148847 84269 71569 85026 130871 102470 53328 63308 104085 57744 123008 120983 94968 69402 83830 137069 121062 71267 103035 97604 129153 65595 148655 124573 139257 59722 101050 139557 74362 50024 101750 83209 117840 139442 127810 113438 94731 125471 96653 88522 125573 74456 89839 84458 128451 68608 92504 103549 117980 126850 144675 59752 60986 125867 89982 108717 134815 89209 143434 61123 103162 139529 122228 137866 78676 80779])


(def total-fuel
  (reduce + (map fuel-required module-masses)))
;;=> 3269199



;; --- Part Two ---
;;
;; During the second Go / No Go poll, the Elf in charge of the Rocket
;; Equation Double-Checker stops the launch sequence. Apparently, you
;; forgot to include additional fuel for the fuel you just added.
;;
;; Fuel itself requires fuel just like a module - take its mass,
;; divide by three, round down, and subtract 2. However, that fuel
;; also requires fuel, and that fuel requires fuel, and so on. Any
;; mass that would require negative fuel should instead be treated as
;; if it requires zero fuel; the remaining mass, if any, is instead
;; handled by wishing really hard, which has no mass and is outside
;; the scope of this calculation.
;;
;; So, for each module mass, calculate its fuel and add it to the
;; total. Then, treat the fuel amount you just calculated as the input
;; mass and repeat the process, continuing until a fuel requirement is
;; zero or negative. For example:
;;
;; A module of mass 14 requires 2 fuel. This fuel requires no further
;; fuel (2 divided by 3 and rounded down is 0, which would call for a
;; negative fuel), so the total fuel required is still just 2.
;;
;; At first, a module of mass 1969 requires 654 fuel. Then, this fuel
;; requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more
;; fuel, which requires 21 fuel, which requires 5 fuel, which requires
;; no further fuel. So, the total fuel required for a module of mass
;; 1969 is 654 + 216 + 70 + 21 + 5 = 966.
;;
;; The fuel required by a module of mass 100756 and its fuel is: 33583
;; + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
;;
;; What is the sum of the fuel requirements for all of the modules on
;; your spacecraft when also taking into account the mass of the added
;; fuel? (Calculate the fuel requirements for each module separately,
;; then add them all up at the end.)


(defn fuel-required-integral
  [mass]
  (->> mass
     (iterate fuel-required)
     (rest)
     (take-while pos?)
     (reduce + )))


(facts
 "account for the fuel weight"

 (fuel-required-integral 1969) => 966
 (fuel-required-integral 100756) => 50346

 )


(def total-fuel-integral
  (reduce + (map fuel-required-integral module-masses)))
;;=> 4900909
