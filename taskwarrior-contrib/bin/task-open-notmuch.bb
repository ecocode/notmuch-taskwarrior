#! /var/home/ec/.asdf/shims/bb

;; This script will open notmuch emacs for the concerned task uuid

(require '[cheshire.core :as json])
(require '[babashka.process :refer [shell process exec]])
(require '[clojure.set :refer [map-invert]])

(defn get-messageIDs-of-task-as-notmuch-search [ uuid ]
  "Returns the messageIDs linked to the task"

  (if (string? uuid)
    (let* [search (str "uuid:" uuid)
           task-list (json/parse-string (:out (shell {:out :string} "task" "rc.json.array=1" search "export")) true)]
      ;; (println "search:" search)
      (when (> (count task-list) 0)
        (let* [task (first task-list)
               messageIDs (:messageID task)
               messageIDs-list (str/split messageIDs #",")
               messageIDs-with-prefix (map #(str "id:" %) messageIDs-list)
               notmuch-search (str "\"(" (str/join " or " messageIDs-with-prefix) ")\"")]
          notmuch-search

          ;; (println messageIDs-list)
          ;; (println notmuch-search)
          )))))

(defn get-notmuch-cmd [notmuch-search]
  "Returns the notmuch cmd command."
  (str "(notmuch-tree " notmuch-search ")"))

;; start of script

(let* [task-uuid (first *command-line-args*)
       notmuch-search (get-messageIDs-of-task-as-notmuch-search task-uuid)
       cmd (get-notmuch-cmd notmuch-search)
       emacs-cmd (str "et -e '" cmd "'")]
  ;; (println cmd)
  ;; (println emacs-cmd)
  (shell emacs-cmd)
  )
