(ns a1202-program-alarm.core
  (:gen-class)
  (:use clojure.pprint))

(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0")

(defn take-while+ [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn read-instruction [state]
  (let [pc (:pc state)]
    (cond (nil? pc) nil
          :else (nth (:program state) pc))))

(defn write-dangerously [state dest value]
  (assoc-in state [:program] (assoc (:program state) dest value)))

(defn execute-instruction  [state]
  (let [instruction (read-instruction state)]
    (cond (= 1 instruction) (let [section (take 3 (drop (inc (:pc state)) (:program state)))
                                  value (reduce + (map #(nth (:program state) %) (take 2 section)))
                                  dest (last section)]
                              (-> state
                                  (update-in [:pc] #(+ 3 %))
                                  (write-dangerously dest value)))
          (= 2 instruction) (let [section (take 3 (drop (inc (:pc state)) (:program state)))
                                  value (reduce * (map #(nth (:program state) %) (take 2 section)))
                                  dest (last section)]
                              (-> state
                                  (update-in [:pc] #(+ 3 %))
                                  (write-dangerously dest value)))
          :else state)))

(defn update-state [state]
  (cond (nil? (:program state))
        (-> state
            (assoc-in [:program] (vec (map (comp #(Integer. %) #(second %)) (re-seq #"(\d+),?" (:input state)))))
            (assoc-in [:pc] 0))
        :else
        (-> state
            (execute-instruction)
            (update-in [:pc] inc))))

(defn adjust-program [noun verb state]
  (let [program (:program state)]
    (assoc-in state [:program]
              (-> program
                  (assoc 1 noun)
                  (assoc 2 verb)))))

(defn compute [state]
  (first (for [noun (range 100) verb (range 100)
               :let [it (->> state
                             (update-state)
                             (adjust-program noun verb)
                             (iterate update-state)
                             (take-while+ #(not= 99 (read-instruction %)))
                             (map (comp first :program))
                             (last))]
               :when (= it 19690720)]
           (+ (* 100 noun) verb))))

(defn -main [& args]
  (println (compute {:input input})))
