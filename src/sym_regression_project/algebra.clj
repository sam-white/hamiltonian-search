(ns sym-regression-project.algebra
(use [clojure.core.match :only (match)])
(use [sym-regression-project.core])
(:require [clojure.zip :as zip]))

(defn cos-approx
  "Pattern matching rule that replaces cos(a) with a small angle approximation."
 [expr]
  (let [ex (vec expr)]
  (match [(first ex) (second ex)]
     ['Math/cos a] (list '- 1 (list '* 0.5 (list '* a a)))
     ['Math/sin a] (list 'Math/sin a)
     [_ _] expr)))

(defn is-cos?
  "Returns true if argument is a seq and begins with Math/cos. Returns false otherwise."
  [expr]
  (if (seq? expr)
    (let [ex (vec expr)]
    (match [(first ex)]
      ['Math/cos] true
      [_] false )) false))

(defn children?
  "Returns true if argument has child nodes."
  [expr]
  (if (not (= nil (zip/children expr)))
    true false))


(defn is-root?
  "Returns true is argument is the root node of an expression."
  [expr]
  (if (= nil (zip/up expr))
    true false))

(defn right-sibling?
  "Returns true if a node has at least one sibling to its right."
  [expr]
  (if (not (= nil (zip/right expr)))
  true false))

(defn traverse-up-tree-cos
  "Recursive function that moves up a tree towards the root. The first subtree of the form
   (Math/cos ...) found is replaced using cos-approx function."
  [expr]
  (if (is-cos? (first expr))
    (zip/root (zip/replace expr (cos-approx (first expr)))) 
              (if (is-root?  expr)
                  (zip/root (zip/replace expr (cos-approx (first expr)))) (traverse-up-tree-cos (zip/up expr)))))

(defn find-bottom-of-tree-cos
  "Recursive function that searches for the bottom of a subtree. When the bottom of the subtree is
   found, traverse-up-tree-cos is called."
  [expr]
  (if (children? expr)
    (find-bottom-of-tree-cos (zip/down expr)) (if (right-sibling? expr)
                                  (find-bottom-of-tree-cos (zip/right expr)) (traverse-up-tree-cos expr))))

(defn find-and-replace-cos
  "Recursive function that replaces the deepest and leftmost subtree of the form (Math/cos ...)
   with the small angle approximation in cos-approx."
  [expr]
  (cond (true? (zip/end? expr)) (zip/root expr)
        (false? (zip/end? expr)) (if (is-cos? (first expr))
                                   (find-bottom-of-tree-cos expr) (find-and-replace-cos (zip/next expr)))))

(defn count-cos
  "Counts the number of occurances of Math/cos in an expression" 
  [expr]
  (count (filter true? (map #(match [%] ['Math/cos] true [_] false) (flatten expr)))))

(defn cos-approx-mutation
  "A mutation operation that replaces every occurance of (Math/cos ...) in an expression
   with a small angle approximation. The replacement rule is applied to the deepest trees
   first."
  [expr]
  (last (take (+ 1 (count-cos expr)) (iterate #(find-and-replace-cos (expr-zip %)) expr))))

(defn apply-cos-approx
  [expr cos-mutate-n]
  (if (< (rand) (/ cos-mutate-n 100))
  (tree-replace expr 0 (cos-approx-mutation expr)) expr)) 



