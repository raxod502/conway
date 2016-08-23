(ns conway.core
  "Contains functions for solving the Slothouber-Graatsma and similar
  puzzles. The general puzzle involves placing a number of differently
  shaped rectangular blocks (in any number of dimensions) onto a
  rectangular grid. The puzzle to be solved is defined by the dynamic
  vars at the top of this namespace (`*grid-shape*`, `*block-shapes*`,
  and `*block-counts*`). Use `solve` to generate a solution.

  Blocks are represented as keywords, which have a shape and count
  assigned to them via the `*block-shapes*` and `*block-counts*` maps.
  The shape of a block is a vector of its side lengths, and the count
  is the number of copies of that block that must be placed in the
  grid. Note that blocks can be oriented in any direction.

  The grid is represented using nested vectors, where to get a
  particular cell you can do (get-in grid [x y z ...]). Each cell will
  either be nil (if it is empty) or a keyword (if it is taken up by
  a block or part of a block)."
  (:require [clojure.pprint :as pp]))

(def ^:dynamic *grid-shape*
  "The side lengths of the grid."
  [3 3 3])
(def ^:dynamic *block-shapes*
  "The side lengths for each type of block."
  {:A [1 1 1]
   :B [1 2 2]})
(def ^:dynamic *block-counts*
  "The number of each type of block that needs to be placed in the grid."
  {:A 3
   :B 6})

(defn initial-grid
  "Returns an empty grid, a nested vector whose shape is given by
  `*grid-shape*` and which contains only nil entries."
  []
  (loop [grid nil
         remaining-shape (reverse *grid-shape*)]
    (if (seq remaining-shape)
      (recur (vec (repeat (first remaining-shape) grid))
             (rest remaining-shape))
      grid)))

(defn local-coordinates
  "Returns a sequence of vectors, each of which represents a set of
  coordinates contained in a rectangular region of the given side
  lengths. All entries are nonnegative, starting at [0 0 ...].
  Inclusive on the zero ends, and exclusive on the positive ends."
  [shape]
  (loop [coords-list [[]]
         shape shape]
    (if (seq shape)
      (recur (mapcat (fn [coords]
                       (map #(conj coords %)
                            (range (first shape))))
                     coords-list)
             (rest shape))
      coords-list)))

(defn try-placing-block-randomly
  "Returns the new grid obtained by placing a block of the given side
  lengths on the given grid, or nil if the random placement is blocked
  by previously placed pieces. The block is denoted on the grid by
  setting each cell it takes up to the given identifier, which must be
  non-nil. If tries is provided, then attempts random placement up to
  that many times before returning nil if all of the placements fail."
  ([grid shape identifier]
   (let [shape (shuffle shape)]
     (when (->> shape
             (map >= *grid-shape*)
             (every? true?))
       (let [corner-coords (mapv (fn [block-length grid-length]
                                   (-> grid-length
                                     (- block-length)
                                     (inc)
                                     (rand-int)))
                                 shape
                                 *grid-shape*)
             block-coords (map #(mapv + corner-coords %)
                               (local-coordinates shape))]
         (when (not-any? #(get-in grid %) block-coords)
           (reduce (fn [grid coords]
                     (assoc-in grid coords identifier))
                   grid
                   block-coords))))))
  ([grid shape identifier tries]
   (->> (try-placing-block-randomly grid shape identifier)
     (fn [])
     (repeatedly tries)
     (first))))

(defn counts->seq
  "Takes a map whose values are nonnegative integers and returns a
  sequence which contains each key the number of times specified by
  the corresponding value, in no particular order. Inverse of
  `clojure.core/frequencies`."
  [freqs]
  (mapcat #(repeat (val %) (key %))
          freqs))

(defn drop-nth
  "Returns a sequence of the collection with the nth element removed."
  [coll n]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn select-random-block
  "Given a sequence of blocks, as represented by keywords, returns a
  map where :block is a random block and :blocks is the rest of the
  blocks."
  [blocks]
  (let [n (rand-int (count blocks))]
    {:block (nth blocks n)
     :blocks (drop-nth blocks n)}))

(defn- solve*
  "Helper function for `solve`. grid is the grid and blocks is a
  sequence of keywords representing the remaining blocks to be
  placed. Returns a grid with all the blocks placed, or nil."
  [placement-tries recur-tries grid blocks]
  (if (seq blocks)
    (->> (let [{:keys [block blocks]}
               (select-random-block blocks)]
           (when-let [grid (try-placing-block-randomly
                             grid (get *block-shapes* block)
                             block placement-tries)]
             (solve* placement-tries recur-tries grid blocks)))
      (fn [])
      (repeatedly recur-tries)
      (remove nil?)
      (first))
    grid))

(defn solve
  "Returns a grid in which all of the blocks have been placed, or nil
  if a solution could not be found. The algorithm used is a recursive
  backtracker: First a block is placed randomly (this is tried until
  a placement succeeds or placement-tries placements have failed), and
  then the algorithm recurs. If this placement does not lead to a
  solution, another random placement is made. This process will repeat
  until a solution is found or recur-tries attempts have been made.

  Solves the puzzle specified by `*grid-shape*`, `*block-shapes*`, and
  `*block-counts*`. Rebind or alter these vars if you want to solve a
  different puzzle.

  Note that since a Monte Carlo algorithm is used, you may have to try
  a few times to get a good solution. Good parameters for the default
  (Slothouber-Graatsma) puzzle are (solve 10000 20)."
  [placement-tries recur-tries]
  (solve* placement-tries
          recur-tries
          (initial-grid)
          (counts->seq *block-counts*)))

(defn -main
  "Solves the Slothouber-Graatsma puzzle, printing the solution. If no
  solution is found, which should be unlikely, prints nil."
  [& args]
  (when (seq args)
    (println "Ignoring arguments:" args))
  (if-let [grid (->> (solve 10000 20)
                  (fn [])
                  (repeatedly 10)
                  (remove nil?)
                  (first))]
    (do
      (println "Each 1x1x1 piece is represented by :A.")
      (println "Each 1x2x2 piece is represented by :B.")
      (pp/pprint grid))))
