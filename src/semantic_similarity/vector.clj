(ns semantic-similarity.vector)

(defn subtract  [vec1 vec2]
  (map - vec1 vec2))

(defn add [vec1 vec2]
  (map + vec1 vec2))

(defn- sq [x] (* x x))

(defn norm [vec]
  (Math/sqrt
    (reduce + (map sq vec))))

(defn normalize [vec]
  (map #(/ %1 (norm vec))
       vec))

(defn dot-product [vec1 vec2]
  (apply + (map * vec1 vec2)))

;; https://en.wikipedia.org/wiki/Matrix_multiplication#Hadamard_product
(defn hadamard-product [vec1 vec2]
  (map * vec1 vec2))
