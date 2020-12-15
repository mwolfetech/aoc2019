(ns monitoring-station.core
  (:gen-class)
  (:require
   [clojure.math.numeric-tower :as ma])
  (:import
   (java.io BufferedReader StringReader)))

(def input "#.#.###.#.#....#..##.#....
.....#..#..#..#.#..#.....#
.##.##.##.##.##..#...#...#
#.#...#.#####...###.#.#.#.
.#####.###.#.#.####.#####.
#.#.#.##.#.##...####.#.##.
##....###..#.#..#..#..###.
..##....#.#...##.#.#...###
#.....#.#######..##.##.#..
#.###.#..###.#.#..##.....#
##.#.#.##.#......#####..##
#..##.#.##..###.##.###..##
#..#.###...#.#...#..#.##.#
.#..#.#....###.#.#..##.#.#
#.##.#####..###...#.###.##
#...##..#..##.##.#.##..###
#.#.###.###.....####.##..#
######....#.##....###.#..#
..##.#.####.....###..##.#.
#..#..#...#.####..######..
#####.##...#.#....#....#.#
.#####.##.#.#####..##.#...
#..##..##.#.##.##.####..##
.##..####..#..####.#######
#.#..#.##.#.######....##..
.#.##.##.####......#.##.##
")

(defn to-bigdec [v]
  (if (zero? v) 0M
      (bigdec v)))

(defn vector-magnitude [x y]
  (to-bigdec (ma/sqrt (+ (ma/expt x 2) (ma/expt y 2)))))

(defn vector-unit [x y magnitude]
  [(with-precision 10  (/ y magnitude)) (with-precision 10  (/ x magnitude))])

(defn to-coordinates [row val]
  (map-indexed (fn [idx itm] (if (= "#" (str itm)) [idx row] nil)) val))

(defn get-coordinates []
  (remove nil? (apply #'concat (map-indexed #'to-coordinates (line-seq (BufferedReader. (StringReader. input)))))))

(defn angle-from-north [v]
  (let [[x y] v
        angle (Math/toDegrees (Math/atan2 (- y) x))]  ;; y values approach 0 as you go north
    (cond (>= 0 angle) (+ 90 (Math/abs angle))
          (>= 90 angle) (- 90 angle)
          (< 90 angle) (+ 270 (- 180 angle)))))

(defn to-unit-vectors [coords]
  (reduce-kv
   (fn [s k v]
     (-> s
         (assoc k  (group-by :angle (sort-by (juxt :angle :magnitude) v)))))
   {}
   (group-by :origin
             ;(seq
              (for [a coords
                        b (remove (fn [c] (and (= (first c) (first a)) (=(second c)(second a)))) coords)]
                    (let [rise  (to-bigdec (- (second b) (second a)))
                          run  (to-bigdec (- (first b) (first a)))
                          magnitude (vector-magnitude rise run)
                          unit-vector (vector-unit rise run magnitude)
                          angle (angle-from-north unit-vector)]
                      {:origin a :dest b :angle angle :magnitude magnitude :unit-vector unit-vector})))))
                                        ;;)

(defn get-counts [m]
  (reduce-kv (fn [s k v]
               (-> s
                   (assoc k (count (distinct (keys v))))))
             {}
             m))

(defn shoot [world to-angle]
  (let [[hit & remaining] (get world to-angle '[])]
       [(assoc world to-angle remaining) hit ]))

(defn -main []
  (let [calc (to-unit-vectors (get-coordinates))
        best-coordinate (first (first (reverse (sort-by  #'second (get-counts calc )))))]
    (loop [
           i 0
           world (get calc best-coordinate)
           angles (cycle (sort (keys world)))]
      (let [angle (first angles)
            [new-world asteroid] (shoot world angle)
            newi (if (nil? asteroid) i (inc i))]
        (if (= 199 i)
          {:asteroid asteroid}
          (recur newi new-world (rest angles)))))))
