(ns space-image-format.core
  (:require [space-image-format.input :refer (input)]
            [clojure.core.matrix :as m]
            [clojure.pprint :refer (pprint)])
  (:gen-class))


(defn parse-input
  ([] (parse-input input))
  ([input-string] 
  (->> (re-seq #"\d" input-string)
       (map #(Integer/parseInt %))
       (vec))))

(defn to-nd-matrix [v width height]
  (let [num-layers (/ (count v) (* width height))]
    (-> v
        (m/matrix)
        (m/reshape [num-layers height width]))))

(defn make-matrix [input-string width height]
  (to-nd-matrix (parse-input input-string) width height))


(defn count-layer-digits
  ([mm digit start end]
    (for [x (range start end)
          :let [c (count (filter #(= digit %)  (flatten (m/get-row mm x))))]]
      c))
  ([mm digit]
   (count-layer-digits mm digit 0 (dec (first (m/shape mm))))))

(defn make-space-image [width height]
  (-> (parse-input)
      (to-nd-matrix width height)))

(defn part-one []
  (let [mm (make-space-image 25 6)
        zero-counts (count-layer-digits mm 0)
        layer (->> zero-counts
                               (map-indexed vector)
                               (apply min-key second)
                               (first))]
    (* (first (count-layer-digits mm 1 layer (inc layer)))
       (first (count-layer-digits mm 2 layer (inc layer))))))

(defn part-two []
  (let [mm (make-space-image 25 6)]
    (-> (for [x (range 0 6)
              y (range 0 25)]
          (first (remove #(= 2 %) (m/select mm :all x y))))
        (m/reshape [6 25]))))

