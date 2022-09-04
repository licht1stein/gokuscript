#!/usr/bin/env bb
;; Copyright (c) Mykhaylo Bilyanskyy, 2022

;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(ns resetvars
  (:require [rewrite-clj.zip :as z]
            [clojure.tools.cli :as tools.cli]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(def logo "
━━━━━━━━┏┓━━━━━━━━━━━━━━━━━━━━━━━━┏┓━
━━━━━━━━┃┃━━━━━━━━━━━━━━━━━━━━━━━┏┛┗┓
┏━━┓┏━━┓┃┃┏┓┏┓┏┓┏━━┓┏━━┓┏━┓┏┓┏━━┓┗┓┏┛
┃┏┓┃┃┏┓┃┃┗┛┛┃┃┃┃┃━━┫┃┏━┛┃┏┛┣┫┃┏┓┃━┃┃━
┃┗┛┃┃┗┛┃┃┏┓┓┃┗┛┃┣━━┃┃┗━┓┃┃━┃┃┃┗┛┃━┃┗┓
┗━┓┃┗━━┛┗┛┗┛┗━━┛┗━━┛┗━━┛┗┛━┗┛┃┏━┛━┗━┛
┏━┛┃━━━━━━━━━━━━━━━━━━━━━━━━━┃┃━━━━━━
┗━━┛━━━━━━━━━━━━━━━━━━━━━━━━━┗┛━━━━━━
")

(def mode-pattern #"layer_[\w\d_]*")

(defn find-layers
  "Find all substrings in s that match the `mode-pattern`. Return a sorted vector of unique matches."
  [s pattern]
  (->> (re-seq pattern s) (into #{}) sort (into [])))

(defn layers->command
  "Take a coll of layers and turn them into an reset command:

  [[\"layer_14c_onepress\" 0] [\"layer_15c_onepress\" 0] ...]"
  [coll]
  (into []
        (for [layer coll]
          [layer 0])))

(defn s->command
  "Parse s for all layer strings and generate command."
  [s pattern]
    (-> s
        (find-layers pattern)
        layers->command))

(defn find-index-by-des [des zloc]
  (->> (z/sexpr zloc)
       (keep-indexed #(when (= (:des %2) des) %1))
       first))

(defn update-rules
  "Get entire karabiner.edn string and update rule with description."
  [s pattern des]
  (let [command (s->command s pattern)
        zloc (z/of-string s)
        main (z/get zloc :main)
        target-ind (find-index-by-des des main)
        reset-rule (z/get main target-ind)]
        (-> reset-rule
        (z/get :rules)
        (z/next)
        (z/next)
        (z/next)
        (z/replace command)
        (z/root-string))))

(defn update-rules-in-file [file & {:keys [pattern des target]}]
  (let [text (slurp file)
        _ (println "Reading file...")
        updated-text (update-rules text pattern des)
        _ (println "Parsing rules...")]
    (println "Updating file...")
    (spit (or target file) updated-text)
    (println "Done.")))

(def cli-options
  ;; An option with a required argument
  [["-d" "--des DESCRIPTION" ":des field to find the right rule to edit"
    :id :des
    :default "Reset all variables"]
   ["-p" "--pattern PATTERN" "Regex pattern to find all variables by"
    :id :pattern
    :default mode-pattern]
   ["-t" "--target-file FILE" "Target file to write to. If not provided write to original file."
    :id :target]
   ["-h" "--help"]])

(defn error [& s]
  (apply println s)
  (System/exit 1))

(defn usage [options-summary]
  (->> ["Gokuscript to automatically generate a command to reset all variables to 0."
        ""
        "Usage: bb gokuscripts.clj FILE [options]"
        ""
        "Options:"
        options-summary
        ""
        "Please refer to the manual page for more information."]
       (str/join \newline)))


(defn -main [& args]
  (println logo)
  (let [{:keys [options arguments errors summary]} (tools.cli/parse-opts args cli-options)
        ;; _ (println options)
        file (first arguments)]

    (cond (:help options) (println (usage summary))
          (not (fs/exists? (fs/file file))) (error "Error! File not found:" file)
          :else (do
                  (println "Updating" file)
                  (update-rules-in-file file options)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
