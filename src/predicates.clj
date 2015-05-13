(ns predicates)

(defn sum-f [f g x]
        (let [f-of-x (f x)
              g-of-x (g x)]
          (+ f-of-x g-of-x)))

(defn less-than [n]
        (fn [k] (< k n)))

(defn equal-to [n]
        (fn [k] (== k n)))

(defn set->predicate [my-set]
        (let [clean-set (fn [x] (if (not x) "nil" x))
              cleaned-set (set(map clean-set my-set))]
          (fn [k] (or
                    (and (cleaned-set "nil") (not k))
                    (cleaned-set k)))))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
 (or (= (count string) 0) (every? whitespace? string)))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [book-has-award? (fn [x] (has-award? book x))]
    (every? book-has-award? awards)))

(defn my-some [pred a-seq]
  (let [my-map (map pred a-seq)
        my-filter (fn [x] (not (= x false)))]
    (filter my-filter my-map)))

(defn my-every? [pred a-seq]
  (let [filtered-seq (filter pred a-seq)
        com-filtered-seq (filter (complement pred) a-seq)]
    (and
     (= (count a-seq) (count filtered-seq))
     (empty? com-filtered-seq))))

(defn prime? [n]
  (let [x-div-by-y (fn [x y] (== (mod x y) 0))
        pred-x-div-by-y (fn [x] (fn [y] (x-div-by-y x y)))
        pot-divs (range 1 n)
        true-divs (filter (pred-x-div-by-y n) pot-divs)]
    (== (count true-divs) 1)))
;^^
