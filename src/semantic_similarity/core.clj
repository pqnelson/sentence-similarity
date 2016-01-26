(ns semantic-similarity.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string :refer [lower-case split]]
            [semantic-similarity.vector :refer :all]))

(def letters #{\a,\b,\c,\d,\e,\f,\g,\h,\i,\j,\k,\l,\m,\n,\o,\p,\q,\r,\s,\t,\u,\v,\w,\x,\y,\z,
               \A,\B,\C,\D,\E,\F,\G,\H,\I,\J,\K,\L,\M,\N,\O,\P,\Q,\R,\S,\T,\U,\V,\W,\X,\Y,\Z})

(defn print-seq [seq]
  (doseq [item seq]
    (println item)))

(defn print-n-return [thing]
  (println thing)
  thing)

(defn get-only-alpha [my-str]
  (string/replace my-str #"[^a-zA-Z]" ""))

(defn get-part-of-speech [word]
  (reduce (fn [acc [letter part-of-speech?]]
            (if part-of-speech? ; part of speech is true  
             (conj acc letter)  ; add the letter to the list
             acc))              ; just return the list
          []
   (let [response (:out (sh "wn" word))]
     (into {} (map #(vector
                     (first %)
                     (.contains response (str "Information available for " (second %))))
                   {"n" "noun" "v" "verb"})))))

(defn get-word-trees [word]
  (map #(:out (sh "wn" word (str "-hype" %))) 
       (get-part-of-speech word))) 

(defn determine-levels [synonym-string]
  (if-not (.contains synonym-string "=>")
    0
    (let [indent-count (.indexOf synonym-string "=>")]
      (/ (- indent-count 3) 4)))) 
 

(defn handle-a-level [state level-string]
  (let [level-num (determine-levels level-string)
        id (inc (state :id))
        cur-path (if (> (count (state :cur-path)) level-num)
                   (conj (subvec (state :cur-path) 0 level-num) id) 
                   (conj (state :cur-path) id))]
  {:id id
   :cur-path cur-path

   :sense 
   (conj (state :sense)
    {:id id 
     :parent (last (butlast cur-path))
     :data (map ;;this will return a level broken into a vector of synonym maps
      (fn [split-synonym] 
        (hash-map 
          :word (get-only-alpha split-synonym)
          :level level-num)) 
      (split level-string #","))})}))


(defn split-str-into-senses [word-tree-string]
  (rest (apply concat (map #(split %1 #"Sense") word-tree-string))))

(defn children-from-parent [sense]
  (reduce 
    (fn [child-struct level] (if (level :parent) 
                               (assoc child-struct 
                                    (level :parent) 
                                    (conj (child-struct (level :parent)) (level :id))) 
                               child-struct ))
     (into (sorted-map) (do (map (fn [level] [(level :id)[]])
      sense))) 
    sense))

(defn depth-from-children-struct [children-struct]
  (loop [depth-struct (sorted-map)
         index (- (count children-struct) 1)]
    (let [new-depth-struct
          (if (> (count (children-struct index)) 0)
            (assoc depth-struct index (+ 1 
                                         (if (> (count (children-struct index)) 1) 
                                         (reduce 
                                            #(if 
                                                (> (depth-struct %1) (depth-struct %2))
                                                (depth-struct %1)
                                                (depth-struct %2))
                                              (children-struct index))
                                         (depth-struct (first (children-struct index))))))
            (assoc depth-struct index 1))]
      (if (= 0 index)
        new-depth-struct  ; terminating condition 
        (recur new-depth-struct (- index 1))))))

(defn insert-depths [strange-data-structure]
  (let [sense (strange-data-structure :sense)]
    (let [ depth-data (depth-from-children-struct
                (children-from-parent sense))]
      (map (fn [sense-level]
          (let [level-id (sense-level :id)]
             (map (fn [word]
                    (assoc word :depth (depth-data level-id)) )
                  (sense-level :data))))
           (strange-data-structure :sense)))))

(defn handle-a-sense [sense]
  (let [split-levels (rest (split sense #"\n"))] 
    (->> split-levels
        (reduce handle-a-level 
                {:id -1 :cur-path[] :sense []})
        (insert-depths) 
        )))

(defn tree-to-level-map [word-tree-string]
  (filter #(contains? %1 :word) 
    (flatten 
      (map 
        handle-a-sense 
        (split-str-into-senses word-tree-string))))) 

(defn tree-contains [tree word]
  (reduce (fn [item1 item2] 
            (if (or item1 (= (:word item2) word))
              true
              false))
         false
         tree))

(defn write-file [thing-to-write]
  (spit "derp.txt" (str thing-to-write "\n") :append true))

(defn group-by-word [tree]
  (into {} (filter (fn [item] (if (> (count (first item)) 0) true false))
   (reduce 
    (fn [groupings node] 
      (if (contains? groupings (node :word))
         (assoc 
           groupings ;map
           (node :word) ;key
           (conj (groupings (node :word)) node)) ;add node to array
         (assoc 
           groupings ;map again
           (node :word) ;key again
           (list node))))
    {}
    tree))))

(defn get-common-ancestors [tree1 tree2]
  (flatten (let [ 
        grouping1 (group-by-word tree1)
        grouping2 (group-by-word tree2)]
    (map 
      (fn [key]
        (map
          (fn [node1]
            (map 
              (fn [node2]
                (assoc node1 :length (+ (node1 :level) (node2 :level))))
              (grouping2 key)))
          (grouping1 key)))
      (keys grouping1)))))

(def ^:const alpha 0.2)
(def ^:const beta 0.45)
(def ^:const e Math/E)

(defn depth-score [depth]
  (/ (- (Math/exp (* beta depth)) 
        (Math/exp (* -1 beta depth)))
     (+ (Math/exp (* beta depth))
        (Math/exp (* -1 beta depth)))))

(defn length-score [length]
  (Math/exp (* -1 alpha length)))

(defn score-item [item]
  (* (depth-score (:depth item))
     (length-score (:length item))))

(defn make-score [tree]
  (apply max
   (map score-item tree)))

;I NEED TO FIND LENGTH
(defn test-semantics [word1 word2]
  (let [tree1 (-> word1 
                  (get-word-trees)
                  (tree-to-level-map))
        tree2 (-> word2
                  (get-word-trees)
                  (tree-to-level-map)) 
        ancestors (get-common-ancestors tree1 tree2)]
    (if (empty? ancestors)
      0
      (make-score ancestors))))

(def ^:const semantic-vector-threshold 0.3)

(defrecord SemanticWord [score
                         word
                         sentence-index])

(defn semantic-max [semantic-structs]
  (first (sort-by :score > semantic-structs)))

(defonce stop-words (split (slurp "resources/stopwords.txt") #"\n"))

(defn stop-word? [s]
  (> (.indexOf stop-words s)))

(defn make-word-score-pair [word branch-sentence]
  (list    
   word
   (semantic-max
    (map
     (fn [branch-word]
       (let [score (test-semantics word branch-word)]
         {:score (cond
                   (> (.indexOf branch-sentence word) -1) 1
                   (stop-word? word)                      0
                   (> score semantic-vector-threshold)    score
                   :else                                  0)
          :w2 branch-word
          :sentence-index 1}))
     (split branch-sentence #" ")))))

;; makes a list of (word best-branch-word-match-plus-score)
(defn get-half-si-vector [base-sentence branch-sentence]
  (map
   (fn [base-word]
     (make-word-score-pair base-word branch-sentence))
   (split base-sentence #" ")))

(defn get-word-counts [si-vector]
  (reduce
    (fn [count-map word-struct]
      (assoc count-map
             (first word-struct)
             (inc (get count-map (first word-struct) 0))))
    {}
    si-vector))

(def ^:const tot-words-in-corpus 329794508)

(defn parse-int  [s]
  (Integer. (re-find #"\d+" s)))

(def frequency-map
  (into {} (map
            (fn [word-&-freq]
              (let  [[word frequency] (split word-&-freq #",")]
                [(lower-case word) (parse-int frequency)]))
            (split (slurp "resources/word-freq.csv") #"\n"))))
 
(defn get-information-content [word-struct si-vec]
  (let [count (frequency-map (first word-struct))]
    (if-not count
      1
      (- 1 
         (/ (Math/log (inc count))
            (Math/log (inc tot-words-in-corpus)))))))

(defn assign-word-order [si-vec joint-word-set]
  (let [jws-as-vec (into [] joint-word-set)]
    (map (fn [[base-word branch-word-match]]
           [base-word
            (assoc branch-word-match
                   :word-order
                   (.indexOf jws-as-vec base-word))])
         si-vec)))

(defn assign-information-content-weight [si-vec]
  (map
   (fn [[base-word branch-match :as item]]
     [base-word
      (assoc branch-match :weight (get-information-content item si-vec))])
   si-vec))

(defn si-to-set [si-vector joint-word-set]
  ; we will count the number of times words occur 
  (into [] (reduce
            (fn [seen [base-word branch-match]]
              (assoc seen
                     base-word
                     (if (contains? seen base-word)
                       (assoc branch-match :sentence-index 3)
                       branch-match)))
            {}
            si-vector)))

(defn get-full-si-vector [sentence1 sentence2 joint-word-set]
  (-> (concat
       (map 
        (fn [word]
          (list word {:score 1 :w2 word :sentence-index 0})) 
        (split sentence1 #" "))
       (get-half-si-vector sentence2 sentence1))
      (assign-information-content-weight)
      (assign-word-order joint-word-set)
      (si-to-set joint-word-set)))
 
(defn filter-by-sentence [si-vec sentence-index]
  (map
    #(if (or (= sentence-index ((second %1) :sentence-index))
             (= 3 ((second %1) :sentence-index)))
       %1 
       (list 
         (first %1)
         (assoc (second %1) :score 0 :word-order 0)))
    si-vec))

(defn extract-from-si-vec [k si-vec]
  (map (comp k second) si-vec))

(defn map-si-vec-to-sentence [sentence si-vec]
  (let [si-map (into {} si-vec )]
    (map #(do
            (list %1
                  (si-map %1)))
         (split sentence #" "))))

(defn order-score [sentence0 si-vec0 sentence1 si-vec1]
  (let 
    [r0 (extract-from-si-vec :word-order 
                             (map-si-vec-to-sentence sentence0 si-vec0))
     r1 (extract-from-si-vec :word-order 
                             (map-si-vec-to-sentence sentence1 si-vec1))]
    (- 1 
       (/ (vec-norm (vec-subtract r0 r1))
          (vec-norm (vec-add r0 r1))))))

(defn semantic-score [base-si-vec branch-si-vec]
  (let [base-score-vec (cross-product (extract-from-si-vec :score si-vec0)
                                      (extract-from-si-vec :weight si-vec0))
        branch-score-vec (cross-product (extract-from-si-vec :score si-vec1)
                                        (extract-from-si-vec :weight si-vec1))]
  (/ (vec-dot-product base-score-vec branch-score-vec)
     (* (vec-norm base-score-vec) (vec-norm branch-score-vec)))))

;; blast off, (you gotta have fun right?)
(def ^:const semantic-over-order 0.7)

(defn <-by-order [item1 item2]
  (if (< (:word-order (second item1)) 
         (:word-order (second item2)))
    true
    false))

(defn joint-words [sentence1 sentence2]
  (apply sorted-set
         (concat 
          (split sentence1 #" ") 
          (split sentence2 #" "))))

(defn get-sentence-similarity [base-sentence branch-sentence]
  (let [sentences [(string/lower-case base-sentence)
                   (string/lower-case branch-sentence)]
        joint-word-set (joint-words (first sentences) (second sentences))
        si-vec0 (sort-by :word-order
                         (get-full-si-vector
                          (first sentences) (second sentences) joint-word-set))
        si-vec1 (sort-by :word-order
                         (get-full-si-vector
                          (second sentences) (first sentences) joint-word-set))]
    (+ (* (- 1 semantic-over-order)
          (order-score 
           (first sentences) si-vec0 
           (second sentences) si-vec1))
       (* semantic-over-order
          (semantic-score si-vec0 si-vec1)))))

