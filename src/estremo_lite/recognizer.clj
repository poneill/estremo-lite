(ns estremo-lite.recognizer
  (:use estremo-lite.utils))

(defn wm-score
  "Score site with weight matrix"
  [wm site]
  (sum (map (fn [[column base]];Destructuring...
              (column base))
            (map vector wm site))))

(defn mlp-score
  "Score site with mlp weights"
  [[input-layer-weights hidden-weights] site]
  (let [intermediates (for [wm input-layer-weights] (wm-score wm site))]
    (sigmoid (sum (map sigmoid intermediates)))))


(defn wm-score-2
  "Score sequence with weight matrix.  NB: loop/recur worse than naive zipping!"
  [wm site]
  (let [n (count site)]
    (loop [acc 0 i 0]
      (if (= i n)
        acc
        (recur (+ acc ((nth wm i) (.charAt site i))) (inc i))))))

(defn wm-scan
  [wm genome]
  (let [n (count genome)
        w (count wm)]
    (map (fn [i] (wm-score wm (slice genome i (+ i w))))
         (range (+ n (- 1 w))))))

(def wm (repeat 10 {\A 1 \C 2 \G 3 \T 4}))
                                        ; consider struct-maps?

(def mlp-wm [(repeat 4 wm) [1 2 3 4]])