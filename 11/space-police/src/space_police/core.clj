(ns space-police.core
    (:gen-class)
     (:require [space-police.base :as b]
               [clojure.core.async :as async]
               [clojure.core.matrix :as mm]
               [clojure.math.numeric-tower :as nn]
               [clojure.pprint :as pp]))

(def input "3,8,1005,8,345,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,28,1006,0,94,2,106,5,10,1,1109,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,62,1,103,6,10,1,108,12,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,92,2,104,18,10,2,1109,2,10,2,1007,5,10,1,7,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,129,2,1004,15,10,2,1103,15,10,2,1009,6,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,164,2,1109,14,10,1,1107,18,10,1,1109,13,10,1,1107,11,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,201,2,104,20,10,1,107,8,10,1,1007,5,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,236,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,257,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,279,1,107,0,10,1,107,16,10,1006,0,24,1,101,3,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,316,2,1108,15,10,2,4,11,10,101,1,9,9,1007,9,934,10,1005,10,15,99,109,667,104,0,104,1,21101,0,936995730328,1,21102,362,1,0,1105,1,466,21102,1,838210728716,1,21101,373,0,0,1105,1,466,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235350789351,1,21101,0,420,0,1105,1,466,21102,29195603035,1,1,21102,1,431,0,1105,1,466,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,825016079204,1,21101,0,454,0,1105,1,466,21101,837896786700,0,1,21102,1,465,0,1106,0,466,99,109,2,21201,-1,0,1,21101,0,40,2,21102,1,497,3,21101,0,487,0,1105,1,530,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,492,493,508,4,0,1001,492,1,492,108,4,492,10,1006,10,524,1101,0,0,492,109,-2,2105,1,0,0,109,4,2102,1,-1,529,1207,-3,0,10,1006,10,547,21102,1,0,-3,21201,-3,0,1,22102,1,-2,2,21101,1,0,3,21102,1,566,0,1105,1,571,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,594,2207,-4,-2,10,1006,10,594,21201,-4,0,-4,1106,0,662,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,613,0,0,1105,1,571,22101,0,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,632,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,654,22101,0,-1,1,21102,654,1,0,105,1,529,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0")

#_(def input "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,39,0,1004,1101,0,37,1013,1101,0,28,1001,1101,0,38,1005,1101,23,0,1008,1102,1,0,1020,1102,1,26,1010,1102,31,1,1009,1101,29,0,1015,1102,459,1,1024,1101,33,0,1007,1101,0,30,1016,1101,32,0,1002,1102,1,494,1027,1101,0,216,1029,1101,497,0,1026,1101,0,303,1022,1102,1,21,1018,1102,1,36,1006,1102,1,27,1014,1102,296,1,1023,1102,454,1,1025,1102,35,1,1003,1101,22,0,1017,1102,225,1,1028,1102,1,20,1011,1101,1,0,1021,1101,0,24,1000,1101,0,25,1019,1101,0,34,1012,109,13,21102,40,1,0,1008,1013,40,63,1005,63,203,4,187,1106,0,207,1001,64,1,64,1002,64,2,64,109,5,2106,0,10,4,213,1001,64,1,64,1105,1,225,1002,64,2,64,109,-3,1206,6,241,1001,64,1,64,1105,1,243,4,231,1002,64,2,64,109,-17,2108,30,4,63,1005,63,259,1106,0,265,4,249,1001,64,1,64,1002,64,2,64,109,14,2108,35,-9,63,1005,63,283,4,271,1105,1,287,1001,64,1,64,1002,64,2,64,109,13,2105,1,-2,1001,64,1,64,1106,0,305,4,293,1002,64,2,64,109,-28,1208,5,32,63,1005,63,327,4,311,1001,64,1,64,1106,0,327,1002,64,2,64,109,12,2102,1,0,63,1008,63,31,63,1005,63,353,4,333,1001,64,1,64,1105,1,353,1002,64,2,64,109,7,21102,41,1,-6,1008,1010,40,63,1005,63,373,1105,1,379,4,359,1001,64,1,64,1002,64,2,64,109,-4,2102,1,-6,63,1008,63,35,63,1005,63,403,1001,64,1,64,1105,1,405,4,385,1002,64,2,64,109,11,21107,42,43,-4,1005,1019,427,4,411,1001,64,1,64,1105,1,427,1002,64,2,64,109,-10,1206,7,445,4,433,1001,64,1,64,1105,1,445,1002,64,2,64,109,10,2105,1,1,4,451,1105,1,463,1001,64,1,64,1002,64,2,64,109,-14,21108,43,42,4,1005,1013,479,1106,0,485,4,469,1001,64,1,64,1002,64,2,64,109,12,2106,0,6,1106,0,503,4,491,1001,64,1,64,1002,64,2,64,109,-10,2107,30,-2,63,1005,63,521,4,509,1106,0,525,1001,64,1,64,1002,64,2,64,109,-7,2101,0,-4,63,1008,63,26,63,1005,63,549,1001,64,1,64,1106,0,551,4,531,1002,64,2,64,109,13,21107,44,43,-3,1005,1014,571,1001,64,1,64,1105,1,573,4,557,1002,64,2,64,109,-6,21108,45,45,1,1005,1012,591,4,579,1106,0,595,1001,64,1,64,1002,64,2,64,109,8,1205,2,609,4,601,1106,0,613,1001,64,1,64,1002,64,2,64,109,-11,1208,-6,34,63,1005,63,629,1106,0,635,4,619,1001,64,1,64,1002,64,2,64,109,-15,2107,33,9,63,1005,63,651,1106,0,657,4,641,1001,64,1,64,1002,64,2,64,109,9,1207,2,38,63,1005,63,677,1001,64,1,64,1106,0,679,4,663,1002,64,2,64,109,8,21101,46,0,0,1008,1010,45,63,1005,63,703,1001,64,1,64,1106,0,705,4,685,1002,64,2,64,109,-5,1201,-3,0,63,1008,63,32,63,1005,63,727,4,711,1106,0,731,1001,64,1,64,1002,64,2,64,109,-6,1207,8,34,63,1005,63,753,4,737,1001,64,1,64,1106,0,753,1002,64,2,64,109,29,1205,-8,765,1106,0,771,4,759,1001,64,1,64,1002,64,2,64,109,-18,1202,-6,1,63,1008,63,39,63,1005,63,797,4,777,1001,64,1,64,1106,0,797,1002,64,2,64,109,8,21101,47,0,0,1008,1018,47,63,1005,63,823,4,803,1001,64,1,64,1105,1,823,1002,64,2,64,109,-12,2101,0,-3,63,1008,63,35,63,1005,63,845,4,829,1106,0,849,1001,64,1,64,1002,64,2,64,109,-9,1201,5,0,63,1008,63,30,63,1005,63,869,1105,1,875,4,855,1001,64,1,64,1002,64,2,64,109,8,1202,-2,1,63,1008,63,34,63,1005,63,899,1001,64,1,64,1105,1,901,4,881,4,64,99,21101,27,0,1,21101,0,915,0,1105,1,922,21201,1,45467,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,942,0,0,1106,0,922,21201,1,0,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,22101,0,-2,-2,109,-3,2106,0,0")

(defn initial-state [program-text in-channel out-channel log-chan]
  {:program-text program-text
   :in in-channel
   :out out-channel
   :log log-chan})

(defn load-input [in-channel val]
  (async/>!! in-channel val))

(defn make-compute-thread [in-channel out-channel log-chan]
  (async/thread (b/compute  (initial-state input in-channel out-channel log-chan)) (async/close! out-channel)(async/close! in-channel)))

(defn paint [panels coords color]
   (assoc panels coords color))


(defn move [coords direction input]
    (case input
      1 (let [new-direction
              (case direction
                [0 1]  [1 0]
                [1 0]  [0 -1]
                [0 -1] [-1 0]
                [-1 0] [0 1])]
          [(mapv + coords new-direction) new-direction])
      0 (let [new-direction
              (case direction
                [0 1]  [-1 0]
                [-1 0] [0 -1]
                [0 -1] [1 0]
                [1 0] [0 1])]
          [(mapv + coords new-direction) new-direction])))
 
(defn run []
  (let [log-chan (async/chan)
        input-channel (async/chan 1)
        output-channel (async/chan 2)
        ship-panels {}]
    (make-compute-thread input-channel output-channel log-chan)
    (load-input input-channel 1)
    (async/thread (loop [] (when-let [v (async/<!! log-chan)]
                             ;;(print v)
                             (recur))))
     (loop [coordinates [0 0] direction [0 1] panels ship-panels]
       (let [color (async/<!! output-channel)]
         (if (nil? color)
           panels
           (let [command (async/<!! output-channel)
                 new-panels (paint panels coordinates color)
                 [new-coordinates new-direction] (move coordinates direction command)]
                          (load-input input-channel (get new-panels new-coordinates 0))
                            (recur new-coordinates new-direction new-panels)))))))
(defn minmax [f coordinates]
  (let [xs (map f coordinates)
        min (apply min xs)
        max (apply max xs)]
    [min max]))

(defn size [[xmin xmax] [ymin ymax]]
  [(nn/abs (- xmax xmin)) (nn/abs (- ymax ymin))])

(defn zeromatrix [width height]
  (mm/zero-matrix (inc width) (inc height)))

(defn read-registration []
  (let [white-coordinates (keys (conj {[0 0] 1} (filter (fn [[_ v]] (odd? v)) (run))))
        domain (minmax #'first white-coordinates)
        range (minmax #'second white-coordinates)
        [width height] (size domain range)
        boostx (- 0 (first domain))
        boosty (- 0 (first range))
        m (zeromatrix width height)]
    (reduce (fn [res [x y]]
              (mm/mset res (+ boostx x) (+ boosty y) nil))
            m
            white-coordinates)))

(defn -main [& _args]
  (run))