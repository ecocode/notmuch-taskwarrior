;;; notmuch-taskwarrior.el --- use taskwarrior from notmuch  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 erik colson

;;; Commentary:
;;; Author: erik colson <eco@ecocode.net>
;;; Version: 0.0.1

;;; Package-Requires: notmuch
;;; Code:

(require 'list-utils)
(require 'dash)
(require 'notmuch)

;; TODO: add support to link a task to multiple mails.  Is there a limit on the length of a UDA field?

(defcustom notmuch-taskwarrior-notmuch-tag "tw/task"
  "Tag added to notmuch mail when a task has been created in taskwarrior."
  :type 'string
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-command "task"
  "Name of taskwarrior executable.  Can include path."
  :type 'string
  :group 'notmuch-taskwarrior)

(defcustom notmuch-taskwarrior-always-tags '("mail")
  "List of tags to always apply for new tasks"
  :type (repeat 'string)
  :group 'notmuch-taskwarrior

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
          (uuid))))

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

(defun notmuch-taskwarrior-new-task-for-mail ()
  "Create a new task with a link to current message-id."
  (interactive)
  (let* ((message-id
          (cond
           ((string-equal major-mode "notmuch-show-mode") (notmuch-show-get-message-id t))
           ((string-equal major-mode "notmuch-tree-mode") (notmuch-tree-get-message-id t))
           ((string-equal major-mode "notmuch-search-mode")
            (progn (message "Creating task not allowed in notmuch-search-mode.") nil))
           (t nil)))
         (message-tags
          (cond
           ((string-equal major-mode "notmuch-show-mode") (notmuch-show-get-tags))
           ((string-equal major-mode "notmuch-tree-mode") (notmuch-tree-get-tags))
           (t nil))))
    (if (not message-id)  ; we are not on a message.
        (message "No message selected.  No task created.")
      (progn (message "Creating task for message %s" message-id)
             (let* ((task-text (read-from-minibuffer "Task: "))
                    (task-project (completing-read "Project: " (notmuch-taskwarrior-get-projects) nil nil (car (notmuch-taskwarrior--filter-tags message-tags))))
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
         (let ((tag-changes (list (notmuch-taskwarrior-notmuch-tag))))
           (notmuch-tag (concat "id:" messageID) tag-changes)
           (message (concat "Added tag" notmuch-taskwarrior-notmuch-tag " to message"))
           (notmuch-tree-tag-update-display tag-changes)))
         (t nil)))

(defun notmuch-taskwarrior--add-taskwarrior-uuid-to-notmuch (messageID uuid)
  "Add task uuid to the notmuch mail with messageID.

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

  ;; TODO: Implement
  )

(defun notmuch-taskwarrior-get-all-ready-tasks ()
  "Return a list of all ready tasks."

  ;;       Retrieve using "task +READY export" -> as json
  ;;       use fields: uuid, project, description

  ;; USE AS FOLLOWS
  ;; (progn
  ;;   (setq x (-flatten-n 1 (notmuch-taskwarrior-get-all-ready-tasks)))
  ;;   (plist-get (plist-get x 2) :text)
  ;; )

  (let* ((tasks (json-parse-string (shell-command-to-string (concat notmuch-taskwarrior-command " " "+READY export")) :object-type 'plist :array-type 'array ))
         (tasklines (cl-map 'array (lambda (x) (list (plist-get x :id) (list :uuid (plist-get x :uuid) :text (concat (plist-get x :project) " " (plist-get x :description))))) tasks)))
    tasklines ; ordered array of uuid and text taskline
    ;; TODO: implement
    ))

(defun notmuch-taskwarrior-link-query-task ()
  "Interactively select a task to link the mail to."

  ;; TODO: Implement

  (interactive)
  )

(provide 'notmuch-taskwarrior)
;;; notmuch-taskwarrior.el ends here
