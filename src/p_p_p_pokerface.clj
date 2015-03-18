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
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
