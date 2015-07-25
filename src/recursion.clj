(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) 
          (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (reduce seq-max nil a-seq))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq) 
      false
    (= elem (first a-seq)) 
      true
    :else 
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (= (pred? (first a-seq)) true)
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
    (= (pred? (first a-seq)) false)
      '()))

(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq) (count b-seq))
      false
    (and (empty? a-seq) (empty? b-seq))
      true
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))
      
(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    (= k 1) n
    :else (* n 
             (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))
    
(defn inits [a-seq]
  (->>
    (reverse a-seq)
    (tails)
    (map reverse)
    ;not required, but it makes the output look nice cuz it's opposite of tails
    (reverse)))

(defn rotations [a-seq]
  (let [result (map concat (tails a-seq) (inits a-seq))]
    ;if more than 1 elem, then the first elem will have a duplicate.
    (if (> (count result) 0)
      (rest result)
      result)))

(defn my-frequencies-helper [freqs a-seq]
  (let [a-seq-key (first a-seq)
        freqs-contains? (contains? freqs a-seq-key)
        recursion (fn [value] (my-frequencies-helper (assoc freqs a-seq-key value) (rest a-seq)))]        
  (cond 
    (empty? a-seq) freqs
    freqs-contains? (recursion (inc (get freqs a-seq-key)))
    :else (recursion 1))))
    
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [first-element (first a-map)
        element-key (first first-element)
        element-value (second first-element)]
    (if (empty? a-map)
      '()
      (concat (repeat element-value element-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons 
      (first coll) 
      (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n)
          (empty? coll))
    (seq coll)
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (conj [] (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))
    
(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) 
    ;using 'into' instead of 'seq' to convert empty data structures into a sequence rather than nil
    (into () a-seq)
    (seq-merge (merge-sort (first (halve a-seq))) 
               (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

