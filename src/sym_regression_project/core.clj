(ns sym-regression-project.core
(:require [clojure.zip :as zip])
(use [clojure repl pprint walk]
     sym-regression-project.symbolic :reload))

;; This program will run a simple symbolic regression using data
;; supplied in interface.clj. Fitness is calculated by taking the chi-squared
;; value of the function formed by the ratio of partial derivatives of a 
;; test-function.

;--------------------------------------------------------------------------------------------------------------------------------------------------
; Basic symbolic regression functions.

;; Define the protected division function.
(defn pdiv [x y] (if (zero? y) 1 (/ x y)))

;; Define set of primitive operators.
(def functions
  [{:name '+ :arity 2}
   {:name '- :arity 2}
   {:name '* :arity 2}
   ;{:name 'Math/sin :arity 1}
   ;{:name 'Math/cos :arity 1}
   ;{:name 'pdiv :arity 2}
     ])

;; Define function terminals (x and a random constant).
(def terminals
  [(constantly 'theta)
   (constantly 'theta-dot)
   ;(constantly 'x)
   ;(constantly 'theta0)
   ;(constantly 'length)
   ;(constantly 'grav-acc)   
   #(* 2 (rand))])


;; Recursive implementation of full tree generation.
(defn random-full-tree
  [functions terminals depth]
  (if (= depth 0)
    ((rand-nth terminals))
    (let [func (rand-nth functions)
          leaves (repeatedly (:arity func) #(random-full-tree functions terminals (- depth 1)))]
      (conj leaves (:name func)))))

;; Function that generates a data set from a given s-expression.
(defn functionalise [ex] (eval (list 'fn '[x] ex)))
(defn functionalise-theta [ex] (eval (list 'fn ['theta 'theta-dot]  ex)))

;; Numerically differentiate data set using a two point evaluation.
(defn vector-coords
  [x y]
  [x y])

(defn numerical-derivative
  "Finds the numerical derivative of a data set."
  [data domain-step]
  (let [y-coords (map #(second %) data) ;f(x)
        x-coords (map #(first %) data) ;x
        y1-values (drop-last 2 y-coords) ;f(x-h)
        y2-values (drop 2 y-coords)  ;f(x+h)
        dy (into [] (map #(- %2 %1) y1-values y2-values))
        dx (into [] (drop-last 1 (drop 1 x-coords)))]  

    (map #(vector-coords %1 %2) dx dy)))

;; Function that generates an initial population (size n) of s-expressions.
(defn make-initial-population
  [n max-depth]
  (repeatedly n #(random-full-tree functions terminals (+ 1 (rand-int (- max-depth 1))))))

;; Generate zipper constructor.
(defn expr-zip
  [expr]
  (zip/zipper
    (constantly true)
    (fn [node] (if (seq? node) (rest node) nil))
    (fn [node children] (with-meta (conj children (first node)) (meta node)))
    expr))

;; Define function that replaces part of a tree with another given tree.
(defn tree-replace
  [tree index new-tree]
  (let [subtree-z (nth (iterate zip/next (expr-zip tree)) index)
        new-zipper (zip/replace subtree-z new-tree)]
    (zip/root new-zipper)))

;; Define function that counts number of nodes.
(defn count-nodes
  [ex]
  (if (seq? ex)
  	(+ 1 (apply + (map count-nodes (rest ex))))
    1))

;; First parameter to be optimised.
(defn score-1a
  "This function calculates how well a candidate expression fits a supplied data set. This is
   calculated using a modified chi squared test. A score of zero refers to an expression
   that perfectly describes the data set. A score much below zero refers to an expression
   that poorly describes the data set."
  [data symbolic-derivative ex]
  (let [d-by-dtheta (symbolic-derivative ex 'theta)
        d-by-dtheta-dot (symbolic-derivative ex 'theta-dot)
        partial-derivs-ratio (list 'pdiv d-by-dtheta-dot d-by-dtheta)
        f (functionalise-theta partial-derivs-ratio)]
    (* -1 (apply + (map #(Math/abs (float (- (f (second %) (nth % 2)) (first %)))) data)))))

(defn score-1b
  "This function calculates how well a candidate expression fits a supplied data set. This is
   calculated using a modified chi squared test. A score of zero refers to an expression
   that perfectly describes the data set. A score much below zero refers to an expression
   that poorly describes the data set."
  [data symbolic-derivative ex]
  (let [d-by-dtheta (symbolic-derivative ex 'theta)
        d-by-dtheta-dot (symbolic-derivative ex 'theta-dot)
        partial-derivs-ratio (list 'pdiv d-by-dtheta d-by-dtheta-dot)
        f (functionalise-theta partial-derivs-ratio)]
    (* -1 (apply + (map #(Math/abs (float (- (f (second %) (nth % 2)) (first %)))) data)))))

;; Second parameter to be optimised.
(defn score-2
  "This function counts the number of nodes in an expression and outputs the negative of
  that number. This is the metric by which the complexity of an expression is determined."
  [count-nodes ex]
  (* -1 (count-nodes ex)))

;; Define mutation operation.
(defn mutate-expr
  [expr new-tree-func]
  (let [size (count-nodes expr)
        target (rand-int size)]
    (tree-replace expr target (new-tree-func))))

;; Define crossover operation
(defn sub-tree
  [tree index]
  (zip/node (nth (iterate zip/next (expr-zip tree)) index)))

(defn crossover-expr
  [expr1 expr2]
  (let [size1 (count-nodes expr1)
        target1 (rand-int size1)
        size2 (count-nodes expr2)
        target2 (rand-int size2)
        subtree1 (sub-tree expr1 target1)
        subtree2 (sub-tree expr2 target2)]
    [(tree-replace expr1 target1 subtree2) (tree-replace expr2 target2 subtree1)]))

;; Map fitness values onto an expression.
(defn score-population
  [population score-1a-func score-1b-func score-2-func]
  (map (fn [expr] {:expr expr 
                   :score-1a (score-1a-func expr)
                   :score-1b (score-1b-func expr)
                   :score-2 (score-2-func expr) 
                   :dominated false
                   :fitness 0})
       population))

(defn symbolic-derivative
  [expr variable]
  (deriv expr variable))

;; Determine if one expression is dominated by another expression. 
(defn is-dominated
  "This function tests whether expression x is dominated by function y.
   An expression is dominated if both score-1 and score-2 of a comparison
   expression is better. "
  [x y]
  (if (or (and (<= (:score-1a x) (:score-1a y)) (< (:score-1b x) (:score-1b y)) (< (:score-2 x) (:score-2 y)))
          (and (< (:score-1a x) (:score-1a y)) (<= (:score-1b x) (:score-1b y)) (< (:score-2 x) (:score-2 y))) 
          (and (< (:score-1a x) (:score-1a y)) (< (:score-1b x) (:score-1b y)) (<= (:score-2 x) (:score-2 y))))
    (assoc x :dominated true) x))

;; Determine if one expression is dominated by another expression (ignoring score 1b). 
(defn is-dominated-1a
  "This function tests whether expression x is dominated by function y.
   An expression is dominated if both score-1 and score-2 of a comparison
   expression is better. "
  [x y]
  (if (or (and (<= (:score-1a x) (:score-1a y)) (< (:score-2 x) (:score-2 y))) 
          (and (< (:score-1a x) (:score-1a y)) (<= (:score-2 x) (:score-2 y))))
     (assoc x :dominated true) x))

;; Determine if one expression is dominated by another expression (ignoring score 1a). 
(defn is-dominated-1b
  "This function tests whether expression x is dominated by function y.
   An expression is dominated if both score-1 and score-2 of a comparison
   expression is better. "
  [x y]
  (if (or (and (<= (:score-1b x) (:score-1b y)) (< (:score-2 x) (:score-2 y))) 
          (and (< (:score-1b x) (:score-1b y)) (<= (:score-2 x) (:score-2 y))))
     (assoc x :dominated true) x))

;; Determine whether a function should go into the archive.
(defn should-be-archived
  "If expression x is dominated by expression y and empty list is
   outputted. If expression x is not dominated, x is outputted."
  [is-dominated x y]
  (if (:dominated (reduce is-dominated x y))
    () x))

(defn into-archive
  "This function maps _should-be-archived_ over every value in
   sequence x. The function outputs a sequence of all expression of x
   that should go into the archive."
  [should-be-archived is-dominated x y]
  (map #(should-be-archived is-dominated % y) x))

(defn remove-repeats
  "Remove expressions that are non-identical yet have the same fitness value."
  [c-archive p-point]
(rand-nth (filter #(and (= (nth p-point 2) (:score-1b %))
                        (= (second p-point) (:score-1a %))
                        (= (first p-point) (:score-2 %))) c-archive)))

(defn extract-coords
  "extract score-1a, score-1b and score-2 from a scored individual and place into a list of coordinates."
  [indv]
 (list (:score-2 indv) (:score-1a indv) (:score-1b indv)))


;--------------------------------------------------------------------------------------------------------------------------------------------------
;; In the SPEA algorithm, fitness is determined by a strength function.
(defn strength-func
  "The strength of an individual in the archive is determined by the number
   of current population members it dominates divided by (1 + population size).
   An archive member's fitness is equal to its strength."
  [archive popn]
  (assoc archive :fitness 
                  (/ 
                   (count 
                    (filter #(and (>= (:score-1a archive) (:score-1a %))
                                  (>= (:score-1b archive) (:score-1b %))
                                  (>= (:score-2 archive) (:score-2 %))) popn))
                    (+ 1 (count popn)))))

(defn sum-strengths
  "This function sums the strengths of all supplied archive members."
  [archive]
  (reduce + (map #(:fitness %) archive)))

(defn fitness-population
  "The fitness of a population individual is determined by 1 + the sum of strengths
   of all the individuals that cover it."
  [popn scored-archive]
  (assoc popn 
    :fitness (+ 1 
              (sum-strengths (filter #(and (<= (:score-1a popn) (:score-1a %)) 
                                           (<= (:score-1b popn) (:score-1b %))
                                           (<= (:score-2 popn) (:score-2 %)))
                                    scored-archive)))))

;; Define tournament selection function. 
(defn tournament-selector
  "This function selects a number of individuals from a population determined by
   tournament-size. The tournament candidate with the minimum fitness value is
   outputted."
  [scored-popn tournament-size]
  (let [competitors (repeatedly tournament-size #(rand-nth scored-popn))]
    (:expr (apply min-key :fitness competitors))))

;--------------------------------------------------------------------------------------------------------------------------------------------------
;; If the archive set is not regularly thinned, the archive will grow without limit.
;; This will lead to a rapid decrease in genetic diversity. The following functions are
;; designed to thin the archive.
(defn distance
  "This function calculates the Euclidean distance between two individuals, x and y."
  [x y]
  (Math/sqrt
   (+ (* (- (:score-1a x) (:score-1a y)) (- (:score-1a x) (:score-1a y)))
      (* (- (:score-1b x) (:score-1b y)) (- (:score-1b x) (:score-1b y)))
      (* (- (:score-2 x) (:score-2 y)) (- (:score-2 x) (:score-2 y))))))

;; gen-map is a map of every expression in the archive and the Euclidean distance
;; between them.
(defn gen-map
  [distance expr-first expr-second]
  {:first-expr (:expr expr-first)
   :second-expr (:expr expr-second)
   :distance (distance expr-first expr-second)})

;; first-map and second-map are required to generate gen-map.
(defn first-map
  [gen-map distance expr-first expr-second]
(map #(gen-map distance % expr-second) expr-first))

(defn second-map
  [first-map distance expr-first expr-second]
(map #(first-map gen-map distance expr-first %) expr-second))

(defn distance-calcs
  "Build gen-map."
  [c-archive]
  (filter #(not (= (:first-expr %) (:second-expr %)))
  (flatten (second-map first-map distance (flatten c-archive) (flatten c-archive)))))

(defn find-min-distance
  "Find the entry in distance-calcs that has the lowest value for distance."
  [distance-calcs c-archive]
  (apply min-key :distance (distance-calcs c-archive)))

;; Generate a new archive set whereby a randomly selected individual from the closest pair
;; of individuals is discarded. Thus, the archive set is reduced by 1.
(defn thin-archive
  "Select new-member by selecting a random individual from closest pair calculated with
  find-min-distance. Remove other member of pair from archive. Add new-member back into the archive."
  [find-min-distance distance-calcs c-archive]
  (let [shortest-distance (find-min-distance distance-calcs c-archive)
        new-member (rand-nth (flatten (map #(if (or (= (:first-expr shortest-distance) (:expr %))
                                                    (= (:second-expr shortest-distance) (:expr %)))
                                                 % ()) c-archive)))]
  (cons new-member
         (filter #(not (or (= (:first-expr shortest-distance) (:expr %))
                           (= (:second-expr shortest-distance) (:expr %)))) c-archive))))

(defn thinning-done
  "Iterate thin-archive function until the total archive set is equal to max-archive-set."
  [thin-archive find-min-distance distance-calcs c-archive archive-size max-archive-size]
  (if (> archive-size max-archive-size)
  (last (take (+ (- archive-size max-archive-size) 1)
        (iterate #(thin-archive find-min-distance distance-calcs %) c-archive))) c-archive))

;-------------------------------------------------------------------------------------------------------
 (defn limit-size
   "A function designed to replace a random subtree with a random constant when the number
    of nodes in an expressions exceeds max-nodes."
   [expr max-nodes]
   (if (> (count-nodes expr) max-nodes)
 (limit-size (tree-replace expr (rand-int (count-nodes expr)) (rand)) max-nodes) expr))

(defn read-from-file [filename]
  "Function that reads from a file specified by filename"
	  (with-open [r (java.io.PushbackReader.
	                 (clojure.java.io/reader filename))]
	    (binding [*read-eval* false]
	      (read r))))












