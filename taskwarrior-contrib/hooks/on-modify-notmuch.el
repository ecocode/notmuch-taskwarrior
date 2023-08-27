#! /var/home/ec/.asdf/shims/bb

;; This script will set notmuch tags to taskwarrior new status for all concerned mails

(require '[cheshire.core :as json])
(require '[babashka.process :refer [shell process exec]])
(require '[clojure.set :refer [map-invert]])

(defn get-message-global-task-status [ messageID excluding-task-uuid new-status]
  "Returns the status for all tasks related to this message"

  ;; order of precedence: pending, waiting, completed, deleted
  ;; so, if one task is pending, return pending etc...
  ;; (println "checking:" messageID)
  (if (string? messageID)
    (let* [filter (str "(messageID.has:" messageID " and uuid.not:" excluding-task-uuid ")")
           status-order {"pending" 1 "waiting" 2 "completed" 3 "deleted" 4 "unknown" 5}
           task-list (json/parse-string (:out (shell {:out :string} "task" "rc.json.array=1" filter "export")) true)
           status-list (cons new-status (map #(get % :status) task-list))
           status-index-list (map #(get status-order %) status-list)
           global-status-index (reduce min 5 status-index-list)
           global-status (get (map-invert status-order) global-status-index)]
      global-status
      )
    new-status
    ))

(defn notmuch-tag-new-status [messageID new-status]
  "retag the notmuch mail for a new taskwarrior status"
  (when (string? messageID)
    (let* [notmuch-tag (str "tw/" (if (string? new-status) new-status "pending"))
           shellout (:out (shell {:out :string}
                                 "/usr/bin/notmuch" "tag" "-tw/task" "-tw/pending" "-tw/waiting" "-tw/completed" "-tw/deleted" "-tw/unknown" (str "+" notmuch-tag) "--" (str "id:" messageID)))]
      (println "tagged message:" messageID " with tag:" notmuch-tag)
      )))


;; Here starts the script

(let [lines (str/split-lines (slurp *in*))
      ;; original-task-json (second lines)
      ;; true will make keywords !
      original-task (json/parse-string (nth lines 0) true)
      modified-task-json (nth lines 1)
      modified-task (json/parse-string modified-task-json true)
      messageIDs (:messageID modified-task)
      status (:status modified-task)
      excluding-task-uuid (:uuid modified-task)  ; we need to exclude original task to check global status
      ]

  (println (str "orig:" original-task))
  (println (str "mod:" modified-task))
  (println (str "messageID:" messageIDs))
  (println (str "status:" status))

  ;; if we have a messageID and modified status is Completed we need to change the notmuch tag
  ;; we need to check if there is no other (pending) task linked to that messageID

  (when (string? messageIDs)
    (let* [messageIDs-list (str/split messageIDs #",")]
      (doseq [m messageIDs-list]
        (notmuch-tag-new-status m (get-message-global-task-status m excluding-task-uuid status)))))

  (println modified-task-json) ; we don't modify the task, but taskwarrior requires that we return this
  (println "Finished retag notmuch mails")
  (System/exit 0)
  )
