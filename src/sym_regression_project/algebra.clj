(ns sym-regression-project.algebra
(use [clojure.core.match :only (match)])
(:require [clojure.zip :as zip]))

(defn cos-approx
 [expr]
  (let [ex (vec expr)]
  (match [(first ex) (second ex)]
     ['Math/cos a] (list '- 1 (list '* 0.5 (list '* a a)))
     ['Math/sin a] (list 'Math/sin a)
     [_ _] expr)))

(defn is-cos?
  [expr]
  (if (seq? expr)
    (let [ex (vec expr)]
    (match [(first ex)]
      ['Math/cos] true
      [_] false )) false))

(defn children?
  [expr]
  (if (not (= nil (zip/children expr)))
    true false))


(defn is-root?
 [expr]
 (if (= nil (zip/up expr))
   true false))

(defn right-sibling?
  [expr]
  (if (not (= nil (zip/right expr)))
  true false))

(defn traverse-up-tree-cos
  [expr]
  (if (is-cos? (first expr))
    (zip/root (zip/replace expr (cos-approx (first expr)))) 
              (if (is-root?  expr)
                  (zip/root (zip/replace expr (cos-approx (first expr)))) (traverse-up-tree-cos (zip/up expr)))))

(defn find-bottom-of-tree
  [expr]
  (if (children? expr)
    (find-bottom-of-tree (zip/down expr)) (if (right-sibling? expr)
                                  (find-bottom-of-tree (zip/right expr)) (traverse-up-tree-cos expr))))

(defn find-and-replace-cos
  [expr]
  (cond (true? (zip/end? expr)) (zip/root expr)
        (false? (zip/end? expr)) (if (is-cos? (first expr))
                                   (find-bottom-of-tree expr) (find-and-replace-cos (zip/next expr)))))



