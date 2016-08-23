(ns conway.core
  "Contains functions for solving the Slothouber-Graatsma and similar
  puzzles. The Slothouber-Graatsma puzzle is to assemble six 1x2x2 and
  three 1x1x1 blocks into a 3x3x3 cube. The general puzzle involves
  placing a number of differently shaped rectangular blocks (in any
  number of dimensions) onto a rectangular grid. The puzzle to be
  solved is defined by the dynamic vars at the top of this
  namespace (`*grid-shape*` and `*block-counts*`). Use `solve` to
  generate a solution.

  Blocks are identified by keywords generated automatically by the
  `block-seq` function based on `*block-counts*`. This var is a map
  from the side lengths of each block to the number of copies of the
  block that must be placed in the grid. Note that blocks can be
  oriented in any direction.

  The grid is represented using nested vectors, where to get a
  particular cell you can do (get-in grid [x y z ...]). Each cell will
  either be nil (if it is empty) or a keyword (if it is taken up by
  a block or part of a block)."
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(def ^:dynamic *grid-shape*
  "The side lengths of the grid."
  [3 3 3])

(def ^:dynamic *block-counts*
  "A map from the side lengths for each block to the number of copies of
  the block that need to be placed in the grid."
  {[1 1 1] 3
   [1 2 2] 6})

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

(def alphabet
  "A string containing the uppercase letters A-Z concatenated."
  (->> (range 26)
    (map #(+ % 65))
    (map char)
    (apply str)))

(def base-identifiers
  "The sequence of identifiers used to identify different types of
  blocks. These have integers postpended to them in order to identify
  individual blocks of the same type."
  alphabet)

(defn block-seq
  "Returns a sequence of maps representing blocks to be placed on
  the grid. Each map has a unique :identifier for the block as well
  as the :shape of the block. The order is guaranteed to be the same
  every time `block-seq` is called."
  []
  (->> *block-counts*
    (sort-by key)
    (mapcat (fn [base-identifer [shape copies]]
              (for [index (range 1 (inc copies))]
                {:identifier (keyword (str base-identifer index))
                 :shape shape}))
            base-identifiers)))

(defn drop-nth
  "Returns a sequence of the collection with the nth element removed."
  [coll n]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn rand-nth-and-rest
  "Given a collection, returns a map containing a random element of
  the collection under :rand-nth and the rest of the collection
  under :rest."
  [coll]
  (let [n (rand-int (count coll))]
    {:rand-nth (nth coll n)
     :rest (drop-nth coll n)}))

(defn- solve*
  "Helper function for `solve`. grid is the partially filled grid and
  blocks is a subsequence of (`block-seq`). Returns a grid with all the
  blocks placed, or nil."
  [placement-tries recur-tries grid blocks]
  (if (seq blocks)
    (->> (let [{block :rand-nth blocks :rest}
               (rand-nth-and-rest blocks)]
           (when-let [grid (try-placing-block-randomly
                             grid (:shape block)
                             (:identifier block) placement-tries)]
             (solve* placement-tries recur-tries grid blocks)))
      (fn [])
      (repeatedly recur-tries)
      (remove nil?)
      (first))
    grid))

(defn solve
  "Returns a grid in which all of the blocks have been placed, or nil
  if a solution could not be found. The algorithm used is a recursive
  backtracker: First a block is placed randomly (using
  `try-placing-block-randomly` with placement-tries), and
  then (provided that a block could be placed) the algorithm recurs.
  If this placement does not lead to a solution, another random
  placement is made. This process will repeat until a solution is
  found or recur-tries attempts have been made at each level.

  Solves the puzzle specified by `*grid-shape*` and `*block-counts*`.
  Rebind or alter these vars if you want to solve a different puzzle.

  Note that since a Monte Carlo algorithm is used, you may have to try
  a few times to get a good solution. Good parameters for the default
  (Slothouber-Graatsma) puzzle are (solve 10000 10)."
  [placement-tries recur-tries]
  (solve* placement-tries
          recur-tries
          (initial-grid)
          (block-seq)))

(defn -main
  "Solves the Slothouber-Graatsma puzzle, printing the solution. If no
  solution is found, which should be unlikely, prints nil."
  [& args]
  (when (seq args)
    (println "Ignoring arguments:" args))
  (if-let [grid (->> (solve 10000 10)
                  (fn [])
                  (repeatedly 10)
                  (remove nil?)
                  (first))]
    (do
      (->> *block-counts*
        (sort-by key)
        (map (fn [base-identifier [shape copies]]
               (printf "%s through %s = %s%n"
                       (keyword (str base-identifier 1))
                       (keyword (str base-identifier copies))
                       (str/join "x" shape))
               (flush))
             base-identifiers)
        (dorun))
      (pp/pprint grid))))
