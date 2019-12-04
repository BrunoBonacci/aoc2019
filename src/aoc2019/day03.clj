(ns aoc2019.day03
  (:require [midje.sweet :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))


;; --- Day 3: Crossed Wires ---
;;
;; The gravity assist was successful, and you're well on your way to
;; the Venus refuelling station. During the rush back on Earth, the
;; fuel management system wasn't completely installed, so that's next
;; on the priority list.
;;
;; Opening the front panel reveals a jumble of wires. Specifically,
;; two wires are connected to a central port and extend outward on a
;; grid. You trace the path each wire takes as it leaves the central
;; port, one wire per line of text (your puzzle input).
;;
;; The wires twist and turn, but the two wires occasionally cross
;; paths. To fix the circuit, you need to find the intersection point
;; closest to the central port. Because the wires are on a grid, use
;; the Manhattan distance for this measurement. While the wires do
;; technically cross right at the central port where they both start,
;; this point does not count, nor does a wire count as crossing with
;; itself.
;;
;; For example, if the first wire's path is R8,U5,L5,D3, then starting
;; from the central port (o), it goes right 8, up 5, left 5, and
;; finally down 3:
;;
;; ```
;; ...........
;; ...........
;; ...........
;; ....+----+.
;; ....|....|.
;; ....|....|.
;; ....|....|.
;; .........|.
;; .o-------+.
;; ...........
;; ```
;;
;; Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right
;; 6, down 4, and left 4:
;;
;; ```
;; ...........
;; .+-----+...
;; .|.....|...
;; .|..+--X-+.
;; .|..|..|.|.
;; .|.-X--+.|.
;; .|..|....|.
;; .|.......|.
;; .o-------+.
;; ...........
;; ```
;;
;; These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.
;;
;; Here are a few more examples:
;;
;; ```
;; R75,D30,R83,U83,L12,D49,R71,U7,L72
;; U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
;; ```
;;
;; ```
;; R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;; U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
;; ```
;;
;; What is the Manhattan distance from the central port to the closest
;; intersection?
;;

(def coefficients
  {"U" [0 -1]
   "D" [0 1]
   "R" [1 0]
   "L" [-1 0]})



(defn step
  [s]
  (->> s
     (re-find #"([UDLR])(\d+)")
     (rest)
     (vec)
     (#(update % 1 read-string))))



(defn move
  [[x y :as last-pos] next-step]
  (let [[dir length] (step next-step)
        [xc yc] (coefficients dir)]
    (->>
     (iterate (fn [[x y]] [(+ x xc) (+ y yc)]) last-pos)
     (take (inc length)))))


(fact
 "go back to start"

 (-> [ 0 0]
    (move "R10")
    last
    (move "U10")
    last
    (move "L10")
    last
    (move "D10")
    last)
 => [0 0])


(defn path
  [path]
  (str/split path #","))


(defn trace-path
  [p]
  (->> p
     (path)
     (reduce (fn [[acc lst] step]
          (let [traces (move lst step)]
            [(concat acc traces) (last traces)]))
        [[] [0 0]])
     first
     dedupe))


(defn crossing
  [path1 path2]
  (set/intersection
   (set path1)
   (set path2)))


(defn abs
  [v]
  (if (neg? v)
    (- v)
    v))


(defn min-manhattan-distance
  [intersections]
  (->> (set/difference intersections #{[0 0]})
     (map (fn [[x y]] (+ (abs x) (abs y))) )
     (reduce min)))


(fact
 "min intersection distance"

 (manhattan-distance
  (crossing
   (trace-path "R75,D30,R83,U83,L12,D49,R71,U7,L72")
   (trace-path "U62,R66,U55,R34,D71,R55,D58,R83")))
 => 159

 (manhattan-distance
  (crossing
   (trace-path "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
   (trace-path "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))
 => 135
 )


(def path1 "R1000,U371,R195,U136,R804,U805,L450,U211,R768,U768,L548,U354,L736,U431,L152,U658,L670,D262,L277,U136,L290,U939,R501,U550,L931,D839,R335,D492,L25,U80,R878,U355,R653,U186,R423,D485,L793,D259,L739,U679,R508,D269,R432,D761,R97,D461,L675,U958,L58,U348,L719,D271,R144,U849,R384,U72,L84,U493,R947,U30,L356,D442,R327,U646,R825,U718,L329,D173,L949,D345,L971,D830,L93,U506,R245,D376,R322,D105,L604,D60,R298,D959,L165,D423,R180,D527,R956,D944,R785,U641,L794,D182,R975,D719,L166,U974,L224,U243,L666,U706,R796,D600,L856,D913,L988,D993,L259,U351,R487,D424,L335,U910,L437,D180,R621,D3,R878,D188,R254,D393,L727,U829,R352,U958,L327,D158,L854,D17,R143,D454,R889,D265,L345,U784,R35,D129,R77,U117,R951,D980,L866,U646,R242,D603,L562,U727,L496,U328,L380,D504,R644,U803,L530,D546,R328,D373,L489,U454,R74,D908,R366,U94,R604,D482,L573,D27,R943,U497,L782,D267,L391,U49,R528,D58,R155,D529,R227,D998,R558,D891,R224,U843,R512,U34,R92,U404,R752,U946,L338,D880,L513,D28,L856,D444,L187,U532,L187,U669,L306,U259,R287,D442,R478,U576,R702,U336,L305,U701,L754,D277,R760,D863,L717,U196,L221,U101,L334,D156,L961,D810,L67,D716,L457,D44,L505,D724,R716,D660,L36,D338,R54,U424,R730,U18,L65,D133,R149,U374,R356,D989,R519,U593,L444,D270,R328,U167,L748,D797,L434,U751,R444,D71,R158,D530,L630,U147,R909,D994,L957,U521,L644,D579,R673,U191,R935,U237,R600,D321,L671,U961,L884,U378,R534,D46,R275,U845,R571,U245,L507,U273,R995,U408,L14,D799,L955,D534,R579,D94,R705,D391,R469,D381,R620,U162,R907,D826,R824,U167,L734,U922,L484")

(def path2 "L1007,D620,R853,U77,L13,U473,L253,D410,R897,U464,L862,U281,L650,D470,R87,D204,L896,U670,L864,D950,L75,D320,R901,D785,L653,D225,L857,U616,L143,U940,L664,U131,L547,D745,R636,U569,L50,U454,R288,D254,L36,U377,L609,U929,L714,U85,L939,U923,L566,D280,R243,U948,R447,D7,R908,D151,R824,D432,R34,D81,L458,U745,L420,D982,L625,U910,L729,D274,R910,U322,L984,D88,L700,D349,L932,U510,R625,U88,L252,U785,L378,D101,R299,U66,L476,U696,R236,D46,R590,U157,R461,U305,L269,D487,L676,U467,R319,D524,R75,U65,L478,U861,L238,D716,R888,D12,L184,D578,R266,D226,L656,D172,L752,U124,L831,U810,L663,U538,R417,D770,L359,U1,R12,U791,L332,U272,R574,D942,L857,U447,R310,U342,L713,D258,R590,D585,R129,D115,R832,D967,R981,D159,R864,U423,R268,U519,L52,D493,R445,D657,R885,U166,R155,D264,R51,D632,R525,D875,R617,U898,L556,D386,L143,U278,L767,D389,R821,U869,R286,D90,R289,U54,R15,D764,R46,D674,R983,U49,R959,U779,R958,D247,R483,U156,L18,U12,L178,U540,L499,U487,L544,D336,R814,U267,R145,D135,L920,D902,L933,D507,L997,U361,L577,U425,L773,D782,R117,U851,R998,U503,R902,U781,L161,U98,L653,U633,L91,U629,L138,D19,R147,D756,R364,D529,L764,U913,L118,U856,R774,D621,R151,U154,R737,D960,R86,U458,R991,D481,R560,D858,R223,D6,R931,D301,R552,D797,R284,U368,L967,D686,R940,U410,R137,D156,L6,U643,L445,D999,R888,D277,L852,U210,L777,D36,R103,D652,R120,D67,L642,D527,R913,D858,R69,D433,R864,U75,L531,U456,L664,D452,R801,U851,L824,D278,L526,U133,R200,U768,R15,U393,R982,U287,L38,D114,R86,U299,L819,D891,R379,D601,L244")

(manhattan-distance
 (crossing
  (trace-path path1)
  (trace-path path2)))
;;=> 489




;; --- Part Two ---
;;
;; It turns out that this circuit is very timing-sensitive; you
;; actually need to minimize the signal delay.
;;
;; To do this, calculate the number of steps each wire takes to reach
;; each intersection; choose the intersection where the sum of both
;; wires' steps is lowest. If a wire visits a position on the grid
;; multiple times, use the steps value from the first time it visits
;; that position when calculating the total value of a specific
;; intersection.
;;
;; The number of steps a wire takes is the total number of grid
;; squares the wire has entered to get to that location, including the
;; intersection being considered. Again consider the example from
;; above:
;;
;; ```
;; ...........
;; .+-----+...
;; .|.....|...
;; .|..+--X-+.
;; .|..|..|.|.
;; .|.-X--+.|.
;; .|..|....|.
;; .|.......|.
;; .o-------+.
;; ...........
;; ```
;;
;; In the above example, the intersection closest to the central port
;; is reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 =
;; 20 steps by the second wire for a total of 20+20 = 40 steps.
;;
;; However, the top-right intersection is better: the first wire takes
;; only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total
;; of 15+15 = 30 steps.
;;
;; Here are the best steps for the extra examples from above:
;;
;; ```
;; R75,D30,R83,U83,L12,D49,R71,U7,L72
;; U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
;; R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;; U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
;; ```
;;
;; What is the fewest combined steps the wires must take to reach an
;; intersection?

(defn min-step-distance
  [path1 path2]
  (let [p1    (trace-path path1)
        p2    (trace-path path2)
        x     (set/difference (crossing p1 p2) #{[0 0]})
        step1 (into {} (map vector p1 (range)))
        step2 (into {} (map vector p2 (range)))
        steps (map (fn [x] (+ (step1 x) (step2 x))) x)]
    (reduce min steps)))

(fact
 "min steps"

 (min-step-distance
  "R8,U5,L5,D3"
  "U7,R6,D4,L4")
 => 30


 (min-step-distance
  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  "U62,R66,U55,R34,D71,R55,D58,R83")
 => 610


 (min-step-distance
  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
 => 410
 )


(min-step-distance
 path1
 path2)
;;=> 93654


;;
