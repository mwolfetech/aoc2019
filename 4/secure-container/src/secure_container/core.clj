(ns secure-container.core
  (:gen-class))

(def input "172930-683082")

(defn to-digits [num]
  (->> (str num)
       seq
       (map (comp read-string str))))

(defn to-inclusive-range
  [str]
  (let [[a b] (map #(Integer/parseInt %) (flatten (re-seq #"\d+" str)))]
    (range a (inc b))))

;; there are better ways but this will work
(defn test-repeats-twice [num]
  (seq (remove #(< 2 (count %)) (map first (re-seq #"(\d)\1+" (str num))))))

(defn test-order [digits]
  (apply <= digits))

(defn check-num [num]
  (let [digits (to-digits num)]
    (and (test-repeats-twice num) (test-order digits))))

(defn -main
  [& args]
  (count (for [x (to-inclusive-range input)
               :when (= (check-num x) true)] x )))
