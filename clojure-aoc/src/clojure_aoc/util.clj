(ns clojure-aoc.util 
  (:require [clojure.java.io :as io]))

(defn read-input [dir]
  (slurp (io/resource (str dir "/input.txt"))))

(defn map2 [f coll]
  (map #(map f %) coll))

(defn descending [x y] (compare y x))