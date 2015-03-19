(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [card-ranks (map rank hand)
        card-frequencies (frequencies card-ranks)]
    (boolean (some #(= 2 %) (vals card-frequencies)))))

(defn three-of-a-kind? [hand]
  (let [card-ranks (map rank hand)
        card-frequencies (frequencies card-ranks)]
    (boolean (some #(= 3 %) (vals card-frequencies)))))

(defn four-of-a-kind? [hand]
  (let [card-ranks (map rank hand)
        card-frequencies (frequencies card-ranks)]
    (boolean (some #(= 4 %) (vals card-frequencies)))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [card-ranks (map rank hand)
        card-frequencies (frequencies card-ranks)]
    (or (four-of-a-kind? hand)
        (=
          (count (filter (fn [[_ v]] (= v 2)) card-frequencies))
          2))))

(defn straight? [hand]
  (let [card-ranks (sort (map rank hand))
        increasing-by-one? (fn
                             [coll]
                             (every? #{1} (map - (rest coll) coll)))]
    (if (some #(= 14 %) card-ranks)
      (let [with-aces (cons 1 card-ranks)]
        (or (increasing-by-one? (butlast with-aces))
            (increasing-by-one? (rest with-aces))))
      (increasing-by-one? card-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[func score]] (if (func hand) score 0)) checkers))))
