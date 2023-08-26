;;; notmuch-taskwarrior.el --- use taskwarrior from notmuch  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2023 erik colson

;;; Author: erik colson <eco@ecocode.net>
;;; Version: 0.0.1
;;; URL: https://www.github.com/ecocode/notmuch-taskwarrior.el
;;; Commentary:

;;; This package allows to create tasks from a notmuch buffer.  The
;;; task will automatically have a metadata MESSAGE_ID.

;;; Package-Requires: (notmuch list-utils dash s)
;;; Code:

;; TODO: add support to link a task to multiple mails.  Is there a limit on the length of a UDA field?

(defgroup notmuch-taskwarrior nil
  "Notmuch to taskwarrior interface"
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-notmuch-tag "tw/pending"
  "Tag added to notmuch mail when a task has been created in taskwarrior."
  :type 'string
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-command "task"
  "Name of taskwarrior executable.  Can include path."
  :type 'string
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-always-tags '("mail")
  "List of tags to always apply for new tasks."
  :type (repeat 'string)
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-regex-use-tags "^[[:digit:]]\\{5\\}"
  "Use tags corresponding to this regex."
  :type 'string
  :group 'notmuch-taskwarrior)

(defun notmuch-taskwarrior-get-projects ()
  "Return list of projects from taskwarrior."
  (delete "" (split-string (shell-command-to-string (concat notmuch-taskwarrior-command " " "_projects")) "[\n\13]+")))

(defun notmuch-taskwarrior-get-tags ()
  "Return list of tags from taskwarrior."
  ;; TODO: we need to remove all CAPITALIZED tags from this list as they are set by taskwarrior
  (delete "" (split-string (shell-command-to-string (concat notmuch-taskwarrior-command " " "_tags")) "[\n\13]+")))

(defun notmuch-taskwarrior-get-latest-task-uuid ()
  "Return the uuid of the latest created taskwarrior task."
  (string-trim (shell-command-to-string (concat notmuch-taskwarrior-command " " "_uuid +LATEST"))))

(defun notmuch-taskwarrior-get-task-uuid (messageID)
  "Retrieve task uuid from taskwarrior for message.  Return nil if none found.

  Arguments:
  - MESSAGEID: message id"

  ;; TODO: it should be ok to have multiple tasks linked to one
  ;; messageID.  Therefor we should return a list of uuids

  (let ((uuid (string-trim (shell-command-to-string (concat notmuch-taskwarrior-command " _uuid messageID:" messageID)))))
    (cond ((string-equal uuid "") nil)
          (t uuid))))

(defun notmuch-taskwarrior--get-task-messageIDs (uuid)
  "Retrieve task messageIDs as list from taskwarrior for task.  Return () if none found.

  Arguments:
  - UUID: task uuid"

  (let* ((cmd (concat notmuch-taskwarrior-command " " (format "uuid:%s" uuid) " export"))
         (shell-output-plist (json-parse-string (shell-command-to-string cmd) :object-type 'plist :array-type 'list ))
         (messageIDs (plist-get (car shell-output-plist) :messageID )))
    (cond ((string-empty-p messageIDs) '())
          (t (split-string messageIDs ",")))))

;; (defun notmuch-taskwarrior-tags-as-string (tag-list)
;;   "Returns a string formatted for task add function."
;;   (string-join (mapcar (lambda (x) (format "+%s" x)) tag-list ) " "))

(defun notmuch-taskwarrior-tags-add-plus (tag-list)
  "Return a list of all tags each preceded with a +.

  Arguments:
  - TAG-LIST: list of tags"

  (mapcar (lambda (x) (format "+%s" x)) tag-list ))

(defun notmuch-taskwarrior--filter-tags (tag-list)
  "Return filtered tag list for new task.

  Arguments:
  - TAG-LIST: list of tags"

  (cl-remove-if-not (lambda (x) (string-match-p notmuch-taskwarrior-regex-use-tags x)) tag-list))

(defun notmuch-taskwarrior--get-messageID ()
  "Get messageID of current message."
  (let* ((message-id
          (cond
           ((string-equal major-mode "notmuch-show-mode") (notmuch-show-get-message-id t))
           ((string-equal major-mode "notmuch-tree-mode") (notmuch-tree-get-message-id t))
           ((string-equal major-mode "notmuch-search-mode")
            (progn (message "Creating task not allowed in notmuch-search-mode.") nil))
           (t nil))))
    message-id))

;;;###autoload
(defun notmuch-taskwarrior-new-task-for-mail ()
  "Create a new task with a link to current message-id."
  (interactive)
  (let* ((message-id (notmuch-taskwarrior--get-messageID))
         (message-tags
          (cond
           ((string-equal major-mode "notmuch-show-mode") (notmuch-show-get-tags))
           ((string-equal major-mode "notmuch-tree-mode") (notmuch-tree-get-tags))
           (t nil))))
    (if (not message-id)  ; we are not on a message.
        (message "No message selected.  No task created.")
      (progn (message "Creating task for message %s" message-id)
             (let* ((task-project (completing-read "Project: " (notmuch-taskwarrior-get-projects) nil nil (car (notmuch-taskwarrior--filter-tags message-tags))))
                    (task-text (read-from-minibuffer "Task: "))
                    (task-tag (completing-read "Tag: " (notmuch-taskwarrior-get-tags) nil nil nil))
                    (task-tags (cons task-tag notmuch-taskwarrior-always-tags)))
               (notmuch-taskwarrior--add-taskwarrior-tag-to-notmuch message-id (notmuch-taskwarrior--shell-new-task task-text message-id task-project task-tags))
               )))))

(defun notmuch-taskwarrior--shell-new-task (description messageID project tags)
  "Create a new task and return it's uuid as a string if successfull or nil if not.

  Arguments:
  - DESCRIPTION: task description
  - MESSAGEID: notmuch message id
  - PROJECT: taskwarrior project
  - TAGS: a list containing all tags to apply"

  ;; TODO: messageID should be a custom UDA field.  Also specify that it must be added in taskrc.

  (let* ((command-args (list-utils-flatten (list "add" (format "messageID:%s" messageID) (format "project:%s" project) (notmuch-taskwarrior-tags-add-plus tags) description)))
         ;; (exitcode (call-process notmuch-taskwarrior-command nil nil nil command-args )))
         (exitcode (apply 'call-process notmuch-taskwarrior-command nil nil nil command-args)))
    (message (concat "COMMAND:" notmuch-taskwarrior-command " " (string-join command-args " ")))
    (cond ((= exitcode 0)(string-trim (shell-command-to-string (concat notmuch-taskwarrior-command " _uuid +LATEST"))))
          (t nil))))

(defun notmuch-taskwarrior--add-taskwarrior-tag-to-notmuch (messageID &optional uuid)
  "Add notmuch-taskwarrior-notmuch-tag to the notmuch mail with messageID.

  Arguments:
  - MESSAGEID: notmuch message id
  - UUID: optional taskwarrior task uuid is allowed, but unused"

  (cond ((and (stringp messageID) (stringp uuid))
         (let ((tag-changes (list (concat "+" notmuch-taskwarrior-notmuch-tag))))
           (notmuch-tag (concat "id:" messageID) tag-changes)
           (message (concat "Added tag" notmuch-taskwarrior-notmuch-tag " to message"))
           (notmuch-tree-tag-update-display tag-changes)))
         (t nil)))

(defun notmuch-taskwarrior--add-taskwarrior-uuid-to-notmuch (messageID uuid)
  "Add task uuid to the notmuch mail with messageID.
  This is not used.

  Arguments:
  - MESSAGEID: notmuch message id
  - UUID: taskwarrior task uuid"

  (cond ((and (stringp messageID) (stringp uuid))
         (let ((tag-changes (list (concat "+task/" uuid))))
           (notmuch-tag (concat "id:" messageID) tag-changes)
           (message (concat "Added task/" uuid " to message"))
           (notmuch-tree-tag-update-display tag-changes)))
        (t nil)))

(defun notmuch-taskwarrior--link-mail-to-task (messageID uuid)
  "Add messageID to task UDA field."

  ;; TODO: allow to add messageID to a list of existing messageIDs
  ;; TODO: be sure messageID is not already linked

  (when (and (not (string-empty-p messageID)) (not (string-empty-p uuid)))

    ;; retrieve messageIDs via taskwarrior as a hash
    ;; add the uuid to the hash (no duplicates)
    ;; use the hash in the command

    (let* ((cur-task-messageID-list (notmuch-taskwarrior--get-task-messageIDs uuid))
           (new-messageID-list (seq-uniq (cons messageID cur-task-messageID-list)))
           (cmd (concat notmuch-taskwarrior-command " " (format "uuid:%s" uuid) " modify " (format "messageID:%s" (s-join "," new-messageID-list)) )))
      (message cmd)
      (shell-command-to-string cmd))))

(defun notmuch-taskwarrior--get-all-ready-tasks ()
  "Return a list of all ready tasks."

  ;;       Retrieve using "task +READY export" -> as json
  ;;       use fields: uuid, project, description, tags

  ;; USE AS FOLLOWS
  ;; (progn
  ;;   (setq x (-flatten-n 1 (notmuch-taskwarrior-get-all-ready-tasks)))
  ;;   (plist-get (plist-get x 2) :text)
  ;; )

  (let* ((tasks (json-parse-string (shell-command-to-string (concat notmuch-taskwarrior-command " " "+READY export")) :object-type 'plist :array-type 'array ))
         (tasklist (cl-map 'array (lambda (x) (list :uuid (plist-get x :uuid) :text (concat (format "%-35.35s" (plist-get x :project)) ": " (format "%-70.70s" (plist-get x :description)) " [" (format "%-30.30s" (string-join (plist-get x :tags) ",")) "] (" (plist-get x :uuid) ")"))) tasks))
         (tasklines (cl-map 'list (lambda (x) (plist-get x :text)) tasklist ))
                                        ; tasklines ; ordered array of uuid and text taskline
      )   tasklines
    ))

;;;###autoload
(defun notmuch-taskwarrior-link-query-task ()
  "Interactively select a task to link the mail to."

  ;; TODO: Implement

  (interactive)
  (let* ((message-id (notmuch-taskwarrior--get-messageID)))
    (when (stringp message-id)
      (let* ((task (completing-read "Task: " (notmuch-taskwarrior--get-all-ready-tasks) nil t nil))
             (task-uuid (car (cdr (car (s-match-strings-all "\(\\([^()]+\\)\)$" task))))))
        (when (stringp task-uuid)
          (notmuch-taskwarrior--link-mail-to-task message-id task-uuid)
          (message (concat "linked message to task " message-id " " task-uuid))
          (notmuch-tree-tag-update-display tag-changes))
        )
      )
    )
  )

(provide 'notmuch-taskwarrior)
;;; notmuch-taskwarrior.el ends here
