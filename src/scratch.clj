(defn equal-to [n]
        (fn [k] (== k n)))

(defn my-some [pred a-seq]
  (let [filtered-seq (filter pred a-seq)]
    (if (> (count filtered-seq) 0) (first filtered-seq) (boolean nil))))
