;;; bc.el --- Basecamp integrated project environment for Emacs -*- lexical-binding: t; -*-

;; Author: Tung Dao <me@tungdao.com>
;; Keywords: project, tools, org, agents

;;; Commentary:
;; Transforms Emacs from a line-by-line code editor into an Integrated Project
;; Environment (HQ) modeled after Basecamp.
;;
;; Features:
;; 1. In-project Basecamp HQ (`PROJECT.org`) with Org dynamic blocks for
;;    To-Dos, Check-ins, Messages/RFCs, and Specs.
;; 2. Rich Global Dashboard (`*Basecamp*` / `bc-dashboard-mode`) aggregating
;;    known projects, live tmux/agent statuses, and cross-project "Hey!" activity.
;; 3. Context-aware capture, actions, and project scaffolding (`bc-init`).
;; 4. RPC handlers for `emacsclient` integration with the `bc` CLI tool and coding agents.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'project)
(require 'subr-x)
(require 'tab-bar)

(declare-function td/command-workspace-open-project-workspace "td-command-workspace" (&optional project-root file))
(declare-function td/command-workspace-open-project-terminal "td-command-workspace" (&optional project-root))
(declare-function td/command-workspace-run-project-agent "td-command-workspace" (prompt &optional project-root))
(declare-function td/command-workspace--tmux-session-exists-p "td-command-workspace" (session-name))
(declare-function td/command-workspace--session-name "td-command-workspace" (project-root))

(defgroup bc nil
  "Basecamp integrated project environment for Emacs."
  :group 'tools
  :prefix "bc-")

;;; Customization

(defcustom bc-project-hq-file "PROJECT.org"
  "Name of the Basecamp HQ file at project root."
  :type 'string
  :group 'bc)

(defcustom bc-docs-directory "docs"
  "Subdirectory containing Basecamp card documents."
  :type 'string
  :group 'bc)

(defcustom bc-tasks-file "docs/TASKS.org"
  "Relative path to the tasks document."
  :type 'string
  :group 'bc)

(defcustom bc-activities-file "docs/ACTIVITIES.org"
  "Relative path to the activities and check-ins document."
  :type 'string
  :group 'bc)

(defcustom bc-messages-file "docs/MESSAGES.org"
  "Relative path to the message board document."
  :type 'string
  :group 'bc)

(defcustom bc-specs-file "docs/SPECS.org"
  "Relative path to the specifications document."
  :type 'string
  :group 'bc)

(defcustom bc-projects-directories '("~/Projects" "~/Projects/personal" "~/Projects/work")
  "Directories scanned for Basecamp projects containing `bc-project-hq-file'."
  :type '(repeat directory)
  :group 'bc)

(defcustom bc-dashboard-refresh-interval 15.0
  "Seconds between automatic refreshes of the global dashboard."
  :type 'number
  :group 'bc)

;;; Faces

(defface bc-dashboard-title
  '((t (:inherit mode-line-buffer-id :weight bold :height 1.3)))
  "Face for dashboard header title."
  :group 'bc)

(defface bc-project-name
  '((t (:inherit font-lock-function-name-face :weight bold :height 1.1)))
  "Face for project names."
  :group 'bc)

(defface bc-project-path
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for project filesystem paths."
  :group 'bc)

(defface bc-section-header
  '((t (:inherit font-lock-keyword-face :weight bold :height 1.1)))
  "Face for dashboard section headers."
  :group 'bc)

(defface bc-status-active
  '((t (:inherit success :weight bold)))
  "Face for active / running statuses."
  :group 'bc)

(defface bc-status-idle
  '((t (:inherit font-lock-constant-face)))
  "Face for idle statuses."
  :group 'bc)

(defface bc-status-todo
  '((t (:inherit warning :weight bold)))
  "Face for TODO task badges."
  :group 'bc)

(defface bc-status-inprogress
  '((t (:inherit font-lock-builtin-face :weight bold)))
  "Face for IN-PROGRESS task badges."
  :group 'bc)

(defface bc-status-done
  '((t (:inherit success)))
  "Face for DONE task badges."
  :group 'bc)

(defface bc-meta
  '((t (:inherit shadow)))
  "Face for metadata and relative timestamps."
  :group 'bc)

(defface bc-action-button
  '((t (:inherit button :weight bold :box (:line-width 1 :color "#444" :style released-button))))
  "Face for action buttons."
  :group 'bc)

;;; Project Discovery & Path Utilities

(defun bc-current-project-root (&optional dir)
  "Find the Basecamp project root for DIR (or `default-directory').
Looks for `bc-project-hq-file' or falls back to `project-current' or .git."
  (let* ((start (file-name-as-directory (expand-file-name (or dir default-directory))))
         (found-hq (locate-dominating-file start bc-project-hq-file)))
    (if found-hq
        (file-name-as-directory (expand-file-name found-hq))
      (if-let* ((proj (project-current nil start)))
          (file-name-as-directory (project-root proj))
        (when-let* ((git-root (locate-dominating-file start ".git")))
          (file-name-as-directory (expand-file-name git-root)))))))

(defun bc-project-p (&optional root)
  "Return non-nil when ROOT (or current directory) is a Basecamp project."
  (let ((resolved (bc-current-project-root root)))
    (and resolved
         (file-exists-p (expand-file-name bc-project-hq-file resolved)))))

(defun bc-project-file (filename &optional root)
  "Return absolute path for relative FILENAME in project ROOT."
  (let ((base (or (bc-current-project-root root) default-directory)))
    (expand-file-name filename base)))

(defun bc-scan-projects ()
  "Return a list of all detected Basecamp project roots."
  (let ((seen (make-hash-table :test #'equal))
        (projects nil))
    ;; 1. Check known project roots from project.el
    (when (fboundp 'project-known-project-roots)
      (dolist (dir (project-known-project-roots))
        (let ((root (file-name-as-directory (expand-file-name dir))))
          (when (and (file-exists-p (expand-file-name bc-project-hq-file root))
                     (not (gethash root seen)))
            (puthash root t seen)
            (push root projects)))))
    ;; 2. Scan bc-projects-directories
    (dolist (parent bc-projects-directories)
      (let ((parent-dir (expand-file-name parent)))
        (when (file-directory-p parent-dir)
          (dolist (entry (directory-files parent-dir t "^[^.]"))
            (when (file-directory-p entry)
              (let ((root (file-name-as-directory (expand-file-name entry))))
                (when (and (file-exists-p (expand-file-name bc-project-hq-file root))
                           (not (gethash root seen)))
                  (puthash root t seen)
                  (push root projects))))))))
    (nreverse projects)))

;;; Relative Time Formatting

(defun bc--parse-iso-time (str)
  "Parse ISO/Org timestamp STR to float unix time."
  (condition-case nil
      (float-time (date-to-time str))
    (error nil)))

(defun bc--format-age (time-val)
  "Return a human-friendly age string for TIME-VAL (float or string)."
  (let* ((timestamp (if (stringp time-val)
                        (bc--parse-iso-time time-val)
                      time-val)))
    (if (not timestamp)
        "recently"
      (let ((diff (max 0 (floor (- (float-time) timestamp)))))
        (cond
         ((< diff 5) "just now")
         ((< diff 60) (format "%ds ago" diff))
         ((< diff 3600) (format "%dm ago" (floor diff 60)))
         ((< diff 86400) (format "%dh %02dm ago"
                                 (floor diff 3600)
                                 (floor (mod diff 3600) 60)))
         ((< diff 604800) (format "%dd ago" (floor diff 86400)))
         (t (format-time-string "%Y-%m-%d" (seconds-to-time timestamp))))))))

;;; Project Scaffolding (`bc-init`)

(defun bc--write-file-if-missing (path content)
  "Write CONTENT to PATH if PATH does not exist."
  (unless (file-exists-p path)
    (let ((dir (file-name-directory path)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file path
      (insert content))))

(defun bc-init (&optional target-dir)
  "Initialize Basecamp structure (`PROJECT.org` + `docs/`) in TARGET-DIR."
  (interactive
   (list (read-directory-name "Initialize Basecamp in directory: "
                              (or (bc-current-project-root) default-directory))))
  (let* ((root (file-name-as-directory (expand-file-name target-dir)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (hq-path (expand-file-name bc-project-hq-file root))
         (tasks-path (expand-file-name bc-tasks-file root))
         (activities-path (expand-file-name bc-activities-file root))
         (messages-path (expand-file-name bc-messages-file root))
         (specs-path (expand-file-name bc-specs-file root)))

    ;; 1. docs/TASKS.org
    (bc--write-file-if-missing
     tasks-path
     (format "#+TITLE: %s — Tasks & Milestones\n#+STARTUP: showall\n#+SEQ_TODO: TODO(t) IN-PROGRESS(i) WAITING(w) | DONE(d) CANCELED(c)\n\n* 🎯 Current Sprint / Milestones\n** TODO Initialize project documentation and setup :setup:\n"
             project-name))

    ;; 2. docs/ACTIVITIES.org
    (bc--write-file-if-missing
     activities-path
     (format "#+TITLE: %s — Activity Log & Check-ins\n#+STARTUP: showall\n\n* %s [Check-in] Project Basecamp Initialized\n:PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\nInitial Basecamp project structure generated.\n"
             project-name
             (format-time-string "[%Y-%m-%d %H:%M]")
             (or user-full-name (user-login-name))
             (format-time-string "%Y-%m-%d %H:%M:%S")))

    ;; 3. docs/MESSAGES.org
    (bc--write-file-if-missing
     messages-path
     (format "#+TITLE: %s — Message Board\n#+STARTUP: showall\n\n* Welcome to %s :announcement:\n:PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\nThis is the project message board for pitches, architectural decisions, and announcements.\n"
             project-name
             project-name
             (or user-full-name (user-login-name))
             (format-time-string "%Y-%m-%d")))

    ;; 4. docs/SPECS.org
    (bc--write-file-if-missing
     specs-path
     (format "#+TITLE: %s — Specifications & Architecture\n#+STARTUP: showall\n\n* Overview\nHigh-level architectural overview and technical contracts for %s.\n"
             project-name
             project-name))

    ;; 5. PROJECT.org (HQ)
    (bc--write-file-if-missing
     hq-path
     (format "#+TITLE: %s — Basecamp HQ\n#+AUTHOR: %s\n#+STARTUP: showall\n\n* 🚀 Project Actions\n[[elisp:(bc-run-agent)][🤖 Run Agent]]  |  [[elisp:(bc-checkin)][💬 Check-in]]  |  [[elisp:(bc-add-task)][✅ New Task]]  |  [[elisp:(bc-open-terminal)][💻 Terminal]]  |  [[elisp:(magit-status)][🐙 Git Status]]  |  [[elisp:(bc-refresh)][🔄 Refresh HQ]]\n\n* 📋 Active To-Dos\n#+BEGIN: bc-tasks :limit 10 :status \"TODO|IN-PROGRESS\"\n#+END:\n\n* 📢 Message Board\n#+BEGIN: bc-messages :limit 5\n#+END:\n\n* ⏱️ Recent Activity & Check-ins\n#+BEGIN: bc-activities :limit 6\n#+END:\n\n* 📚 Docs & Specifications\n#+BEGIN: bc-specs :dir \"docs\"\n#+END:\n"
             project-name
             (or user-full-name "Tung Dao")))

    ;; Remember project root
    (when-let* (((fboundp 'project-remember-project))
                (proj (project-current nil root)))
      (project-remember-project proj))

    (message "Initialized Basecamp HQ for %s in %s" project-name root)
    (find-file hq-path)
    (bc-refresh)))

;;; Org File Parsers & Extractors

(defun bc--match-status-p (filter status)
  "Return non-nil if STATUS matches FILTER (e.g. \"TODO|IN-PROGRESS\" or \"TODO\")."
  (if (or (null filter) (string-empty-p filter))
      t
    (let ((parts (split-string filter "[|,]" t "[ \t\n\r]+")))
      (member (upcase (or status "")) (mapcar #'upcase parts)))))

(defun bc--extract-tasks (&optional root-or-dir status-filter)
  "Extract task plists from TASKS.org in ROOT-OR-DIR.
Filters by STATUS-FILTER (e.g. \"TODO|IN-PROGRESS\") if provided."
  (let* ((tasks-path (bc-project-file bc-tasks-file root-or-dir))
         (tasks nil))
    (when (file-exists-p tasks-path)
      (with-temp-buffer
        (insert-file-contents tasks-path)
        (org-mode)
        (org-set-regexps-and-options)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (let ((todo (org-element-property :todo-keyword hl))
                  (title (org-element-property :raw-value hl))
                  (priority (org-element-property :priority hl))
                  (tags (org-element-property :tags hl))
                  (begin (org-element-property :begin hl)))
              (when todo
                (when (bc--match-status-p status-filter todo)
                  (push (list :title title
                              :status todo
                              :priority (and priority (char-to-string priority))
                              :tags tags
                              :position begin
                              :file tasks-path)
                        tasks))))
            nil))))
    (nreverse tasks)))

(defun bc--extract-messages (&optional root-or-dir category-filter)
  "Extract message threads from MESSAGES.org in ROOT-OR-DIR."
  (let* ((msg-path (bc-project-file bc-messages-file root-or-dir))
         (messages nil))
    (when (file-exists-p msg-path)
      (with-temp-buffer
        (insert-file-contents msg-path)
        (org-mode)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (when (= (org-element-property :level hl) 1)
              (let* ((title (org-element-property :raw-value hl))
                     (tags (org-element-property :tags hl))
                     (author (org-element-property :AUTHOR hl))
                     (date (org-element-property :DATE hl))
                     (begin (org-element-property :begin hl)))
                (when (or (null category-filter)
                          (seq-some (lambda (tg) (string-match-p category-filter tg)) tags))
                  (push (list :title title
                              :tags tags
                              :author (or author "Unknown")
                              :date (or date "")
                              :position begin
                              :file msg-path)
                        messages))))
            nil))))
    (nreverse messages)))

(defun bc--extract-activities (&optional root-or-dir limit)
  "Extract check-in and activity entries from ACTIVITIES.org in ROOT-OR-DIR."
  (let* ((act-path (bc-project-file bc-activities-file root-or-dir))
         (activities nil))
    (when (file-exists-p act-path)
      (with-temp-buffer
        (insert-file-contents act-path)
        (org-mode)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (when (= (org-element-property :level hl) 1)
              (let* ((title (org-element-property :raw-value hl))
                     (author (org-element-property :AUTHOR hl))
                     (date (org-element-property :DATE hl))
                     (begin (org-element-property :begin hl)))
                (push (list :title title
                            :author (or author "")
                            :date (or date "")
                            :position begin
                            :file act-path)
                      activities)))
            nil))))
    (let ((res (nreverse activities)))
      (if (and limit (> (length res) limit))
          (seq-take res limit)
        res))))

;;; Org Dynamic Blocks Formatters

(defun org-dblock-write:bc-tasks (params)
  "Dynamic block writer for Basecamp tasks."
  (let* ((root (bc-current-project-root))
         (limit (or (plist-get params :limit) 15))
         (status-filter (plist-get params :status))
         (tasks (bc--extract-tasks root status-filter))
         (tasks-subset (if limit (seq-take tasks limit) tasks)))
    (if (null tasks-subset)
        (insert "  /No active tasks match current filter./\n")
      (insert "| Status | Pri | Task | Tags |\n")
      (insert "|--------+-----+------+------|\n")
      (dolist (task tasks-subset)
        (let* ((status (plist-get task :status))
               (priority (or (plist-get task :priority) ""))
               (title (plist-get task :title))
               (tags (plist-get task :tags))
               (tag-str (if tags (string-join tags ":") ""))
               (clean-title (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" title))
               (link (format "[[file:docs/TASKS.org::*%s][%s]]" clean-title clean-title)))
          (insert (format "| %s | %s | %s | %s |\n"
                          status priority link tag-str))))
      (org-table-align))))

(defun org-dblock-write:bc-messages (params)
  "Dynamic block writer for Basecamp messages."
  (let* ((root (bc-current-project-root))
         (limit (or (plist-get params :limit) 5))
         (category (plist-get params :category))
         (messages (bc--extract-messages root category))
         (msg-subset (if limit (seq-take messages limit) messages)))
    (if (null msg-subset)
        (insert "  /No messages posted yet. Use =bc-post-message= (C-c b m) to post a pitch or RFC./\n")
      (dolist (msg msg-subset)
        (let* ((title (plist-get msg :title))
               (author (plist-get msg :author))
               (date (plist-get msg :date))
               (tags (plist-get msg :tags))
               (tag-str (if tags (format " :%s:" (string-join tags ":")) ""))
               (link (format "[[file:docs/MESSAGES.org::*%s][%s]]" title title)))
          (insert (format "- %s%s  /(%s, %s)/\n" link tag-str author date)))))))

(defun org-dblock-write:bc-activities (params)
  "Dynamic block writer for Basecamp activities and check-ins."
  (let* ((root (bc-current-project-root))
         (limit (or (plist-get params :limit) 6))
         (acts (bc--extract-activities root limit)))
    (if (null acts)
        (insert "  /No check-ins logged yet. Use =bc-checkin= (C-c b i) to post an update./\n")
      (dolist (act acts)
        (let* ((title (plist-get act :title))
               (author (plist-get act :author))
               (date (plist-get act :date))
               (link (format "[[file:docs/ACTIVITIES.org::*%s][%s]]" title title)))
          (insert (format "- %s  /(%s, %s)/\n"
                          link
                          (if (string-empty-p author) "Tung" author)
                          (if (string-empty-p date) "recent" (bc--format-age date)))))))))

(defun org-dblock-write:bc-specs (_params)
  "Dynamic block writer for Basecamp specifications."
  (let* ((root (bc-current-project-root))
         (docs-dir (expand-file-name bc-docs-directory root))
         (files (and (file-directory-p docs-dir)
                     (directory-files docs-dir nil "\\.\\(org\\|md\\)$"))))
    (if (null files)
        (insert "  /No specification documents found in docs/ folder./\n")
      (dolist (file files)
        (unless (member file '("TASKS.org" "ACTIVITIES.org" "MESSAGES.org"))
          (insert (format "- [[file:docs/%s][%s]]\n" file file)))))))

;;; Interactive Card Actions

(defun bc-refresh ()
  "Refresh all dynamic blocks in `PROJECT.org` and update global dashboard."
  (interactive)
  (let ((hq (bc-project-file bc-project-hq-file)))
    (when (file-exists-p hq)
      (with-current-buffer (find-file-noselect hq)
        (org-set-regexps-and-options)
        (org-update-all-dblocks)
        (save-buffer))))
  ;; Also refresh global dashboard if live
  (when-let* ((dash-buf (get-buffer "*Basecamp*")))
    (when (buffer-live-p dash-buf)
      (with-current-buffer dash-buf
        (when (eq major-mode 'bc-dashboard-mode)
          (bc-dashboard-refresh)))))
  (message "Basecamp HQ refreshed."))

(defun bc-add-task (title &optional priority tags body root-or-dir)
  "Add a new task to `docs/TASKS.org` for ROOT-OR-DIR."
  (interactive
   (list (read-string "Task title: ")
         (read-string "Priority (A, B, C or empty): ")
         (read-string "Tags (colon-separated, e.g. feat:ui): ")))
  (let* ((root (bc-current-project-root root-or-dir))
         (tasks-path (expand-file-name bc-tasks-file root)))
    (unless (file-exists-p tasks-path)
      (user-error "Tasks file %s does not exist. Run M-x bc-init first" tasks-path))
    (with-current-buffer (find-file-noselect tasks-path)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((pri-str (if (and priority (not (string-empty-p priority)))
                         (format "[#%s] " (upcase priority))
                       ""))
            (tag-str (if (and tags (not (string-empty-p tags)))
                         (format " :%s:" (string-trim tags ":"))
                       "")))
        (insert (format "* TODO %s%s%s\n" pri-str title tag-str)))
      (when (and body (not (string-empty-p body)))
        (insert (format "%s\n" body)))
      (save-buffer))
    (let ((default-directory root))
      (bc-refresh))
    (message "Added task: %s" title)))

(defun bc-checkin (summary &optional body agent-name root-or-dir)
  "Append an activity check-in to `docs/ACTIVITIES.org`."
  (interactive
   (list (read-string "Check-in summary: ")))
  (let* ((root (bc-current-project-root root-or-dir))
         (act-path (expand-file-name bc-activities-file root))
         (author (or agent-name user-full-name (user-login-name) "Tung Dao"))
         (time-stamp (format-time-string "[%Y-%m-%d %H:%M]"))
         (iso-date (format-time-string "%Y-%m-%d %H:%M:%S")))
    (unless (file-exists-p act-path)
      (user-error "Activities file %s does not exist. Run M-x bc-init first" act-path))
    (with-current-buffer (find-file-noselect act-path)
      (goto-char (point-min))
      ;; Insert after header comments
      (if (re-search-forward "^\\* " nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max)))
      (insert (format "* %s %s\n:PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\n"
                      time-stamp summary author iso-date))
      (when (and body (not (string-empty-p body)))
        (insert (format "%s\n" body)))
      (insert "\n")
      (save-buffer))
    (let ((default-directory root))
      (bc-refresh))
    (message "Logged check-in: %s" summary)))

(defun bc-post-message (title category body &optional root-or-dir)
  "Post a pitch/RFC/decision to `docs/MESSAGES.org`."
  (interactive
   (list (read-string "Message title: ")
         (completing-read "Category: " '("pitch" "rfc" "decision" "announcement" "discussion") nil t "pitch")
         (read-string "Body: ")))
  (let* ((root (bc-current-project-root root-or-dir))
         (msg-path (expand-file-name bc-messages-file root))
         (author (or user-full-name (user-login-name) "Tung Dao"))
         (iso-date (format-time-string "%Y-%m-%d %H:%M:%S")))
    (unless (file-exists-p msg-path)
      (user-error "Messages file %s does not exist. Run M-x bc-init first" msg-path))
    (with-current-buffer (find-file-noselect msg-path)
      (goto-char (point-min))
      (if (re-search-forward "^\\* " nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max)))
      (insert (format "* %s :%s:\n:PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\n%s\n\n"
                      title category author iso-date body))
      (save-buffer))
    (let ((default-directory root))
      (bc-refresh))
    (message "Posted %s: %s" category title)))

(defun bc-open-hq (&optional project-root)
  "Open the Basecamp HQ (`PROJECT.org`) for PROJECT-ROOT in its workspace."
  (interactive)
  (let* ((root (or project-root (bc-current-project-root)))
         (hq-file (expand-file-name bc-project-hq-file root)))
    (unless (file-exists-p hq-file)
      (when (y-or-n-p (format "No Basecamp HQ in %s. Initialize now? " root))
        (bc-init root)))
    (if (fboundp 'td/command-workspace-open-project-workspace)
        (td/command-workspace-open-project-workspace root hq-file)
      (find-file hq-file))))

(defun bc-run-agent (&optional prompt project-root)
  "Dispatch a coding agent in PROJECT-ROOT."
  (interactive)
  (let* ((root (or project-root (bc-current-project-root)))
         (p (or prompt (read-string "Agent Prompt: "))))
    (if (fboundp 'td/command-workspace-run-project-agent)
        (td/command-workspace-run-project-agent p root)
      (message "Running agent: %s (in %s)" p root))))

(defun bc-open-terminal (&optional project-root)
  "Open terminal for PROJECT-ROOT."
  (interactive)
  (let ((root (or project-root (bc-current-project-root))))
    (if (fboundp 'td/command-workspace-open-project-terminal)
        (td/command-workspace-open-project-terminal root)
      (dired root))))

;;; Global Basecamp Dashboard (`*Basecamp*`)

(defvar bc-dashboard-buffer-name "*Basecamp*"
  "Buffer name for the global Basecamp dashboard.")

(defvar-local bc-dashboard--refresh-timer nil
  "Timer for automatic dashboard refreshes.")

(defvar bc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'bc-dashboard-open-selected)
    (define-key map (kbd "n")   #'bc-dashboard-next)
    (define-key map (kbd "p")   #'bc-dashboard-previous)
    (define-key map (kbd "a")   #'bc-dashboard-run-agent)
    (define-key map (kbd "t")   #'bc-dashboard-open-terminal)
    (define-key map (kbd "c")   #'bc-dashboard-checkin)
    (define-key map (kbd "+")   #'bc-dashboard-init-project)
    (define-key map (kbd "i")   #'bc-dashboard-init-project)
    (define-key map (kbd "g")   #'bc-dashboard-refresh)
    (define-key map (kbd "r")   #'bc-dashboard-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `bc-dashboard-mode'.")

(define-derived-mode bc-dashboard-mode special-mode "Basecamp"
  "Major mode for the Basecamp global dashboard."
  :group 'bc
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

(defun bc-dashboard-next ()
  "Move point to next project card."
  (interactive)
  (let ((pos (next-single-property-change (point) 'bc-project-root)))
    (if pos
        (goto-char pos)
      (goto-char (point-min))
      (if-let* ((first (next-single-property-change (point) 'bc-project-root)))
          (goto-char first)))))

(defun bc-dashboard-previous ()
  "Move point to previous project card."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'bc-project-root)))
    (if pos
        (goto-char pos)
      (goto-char (point-max))
      (if-let* ((last (previous-single-property-change (point) 'bc-project-root)))
          (goto-char last)))))

(defun bc-dashboard-current-project ()
  "Return project root at point in dashboard."
  (get-text-property (point) 'bc-project-root))

(defun bc-dashboard-open-selected ()
  "Open the project HQ for project under point."
  (interactive)
  (if-let* ((root (bc-dashboard-current-project)))
      (bc-open-hq root)
    (user-error "No project at point")))

(defun bc-dashboard-run-agent ()
  "Run agent in project under point."
  (interactive)
  (if-let* ((root (bc-dashboard-current-project)))
      (bc-run-agent nil root)
    (user-error "No project at point")))

(defun bc-dashboard-open-terminal ()
  "Open terminal for project under point."
  (interactive)
  (if-let* ((root (bc-dashboard-current-project)))
      (bc-open-terminal root)
    (user-error "No project at point")))

(defun bc-dashboard-checkin ()
  "Post check-in to project under point."
  (interactive)
  (if-let* ((root (bc-dashboard-current-project)))
      (call-interactively
       (lambda (summary)
         (interactive "sCheck-in summary: ")
         (bc-checkin summary nil nil root)))
    (user-error "No project at point")))

(defun bc-dashboard-init-project ()
  "Initialize a new Basecamp project."
  (interactive)
  (call-interactively #'bc-init))

(defun bc--tmux-status (root)
  "Return tmux status string and face for ROOT."
  (if (fboundp 'td/command-workspace--session-name)
      (let ((sess (td/command-workspace--session-name root)))
        (if (and (fboundp 'td/command-workspace--tmux-session-exists-p)
                 (td/command-workspace--tmux-session-exists-p sess))
            (cons "[tmux: active]" 'bc-status-active)
          (cons "[no tmux]" 'bc-meta)))
    (cons "" 'bc-meta)))

(defun bc--project-metrics (root)
  "Return plist of counts and last activity for project ROOT."
  (let* ((tasks (bc--extract-tasks root))
         (todo-cnt (seq-count (lambda (x) (string-equal (plist-get x :status) "TODO")) tasks))
         (inprog-cnt (seq-count (lambda (x) (string-equal (plist-get x :status) "IN-PROGRESS")) tasks))
         (done-cnt (seq-count (lambda (x) (string-equal (plist-get x :status) "DONE")) tasks))
         (acts (bc--extract-activities root 1))
         (last-act (car acts)))
    (list :todo todo-cnt
          :inprog inprog-cnt
          :done done-cnt
          :last-activity last-act)))

(defun bc-dashboard-render ()
  "Render the `*Basecamp*` dashboard buffer."
  (let* ((projects (bc-scan-projects))
         (inhibit-read-only t)
         (saved-point (point))
         (all-activities nil))
    (erase-buffer)
    ;; Header
    (insert (propertize " ⬢ BASECAMP HQ " 'face 'bc-dashboard-title))
    (insert (propertize (format "  —  %d Active Projects" (length projects)) 'face 'bc-meta))
    (insert (propertize (format "  (Updated %s)" (format-time-string "%H:%M:%S")) 'face 'bc-meta))
    (insert "\n")
    (insert (propertize (make-string 88 ?─) 'face 'bc-meta))
    (insert "\n\n")

    ;; Project Cards
    (insert (propertize " PROJECTS\n" 'face 'bc-section-header))
    (if (null projects)
        (insert "  No Basecamp projects found. Press '+' or 'i' to initialize a project.\n\n")
      (dolist (root projects)
        (let* ((name (file-name-nondirectory (directory-file-name root)))
               (tmux (bc--tmux-status root))
               (metrics (bc--project-metrics root))
               (todo (plist-get metrics :todo))
               (inprog (plist-get metrics :inprog))
               (done (plist-get metrics :done))
               (last-act (plist-get metrics :last-activity))
               (acts (bc--extract-activities root 3)))
          ;; Collect for cross-project activity feed
          (dolist (a acts)
            (push (cons root a) all-activities))

          ;; Project Card Item
          (let ((card-start (point)))
            (insert (format "  • %-24s" name))
            (put-text-property card-start (point) 'face 'bc-project-name)
            (insert (propertize (format "%-18s" (car tmux)) 'face (cdr tmux)))
            (insert "\n")
            (insert (propertize (format "    %-32s" (abbreviate-file-name root)) 'face 'bc-project-path))
            (insert (format "  %s  %s  %s\n"
                            (propertize (format "%d TODO" todo) 'face 'bc-status-todo)
                            (propertize (format "%d IN-PROGRESS" inprog) 'face 'bc-status-inprogress)
                            (propertize (format "%d DONE" done) 'face 'bc-status-done)))
            (if last-act
                (let* ((title (plist-get last-act :title))
                       (date (plist-get last-act :date))
                       (age (if (string-empty-p date) "" (format " (%s)" (bc--format-age date)))))
                  (insert (propertize "    Latest Check-in: " 'face 'bc-meta))
                  (insert (format "\"%s\"%s\n\n" title (propertize age 'face 'bc-meta))))
              (insert (propertize "    Latest Check-in:  (no check-ins yet)\n\n" 'face 'bc-meta)))
            (put-text-property card-start (point) 'bc-project-root root)))))

    ;; Cross-Project "Hey!" Activity Stream
    (insert (propertize (make-string 88 ?─) 'face 'bc-meta))
    (insert "\n")
    (insert (propertize " RECENT ACTIVITY & CHECK-INS (\"Hey!\" Stream)\n" 'face 'bc-section-header))
    (if (null all-activities)
        (insert "  No recent activity logged across projects.\n\n")
      (let* ((sorted (seq-take
                      (sort all-activities
                            (lambda (a b)
                              (string> (or (plist-get (cdr a) :date) "")
                                       (or (plist-get (cdr b) :date) ""))))
                      8)))
        (dolist (item sorted)
          (let* ((root (car item))
                 (act (cdr item))
                 (proj-name (file-name-nondirectory (directory-file-name root)))
                 (title (plist-get act :title))
                 (author (plist-get act :author))
                 (date (plist-get act :date))
                 (age (if (string-empty-p date) "recent" (bc--format-age date))))
            (insert (format "  %-12s %-16s %-10s %s\n"
                            (propertize age 'face 'bc-meta)
                            (propertize proj-name 'face 'bc-project-name)
                            (propertize (if (string-empty-p author) "Tung" author) 'face 'bc-meta)
                            title))))
        (insert "\n")))

    ;; Footer Navigation Legend
    (insert (propertize (make-string 88 ?─) 'face 'bc-meta))
    (insert "\n")
    (insert " [RET] Open HQ   [a] Run Agent   [t] Terminal   [c] Check-in   [+] Init Project   [g] Refresh\n")

    (goto-char (min saved-point (point-max)))))

(defun bc-dashboard-refresh ()
  "Refresh the Basecamp global dashboard."
  (interactive)
  (let ((buf (get-buffer-create bc-dashboard-buffer-name)))
    (with-current-buffer buf
      (bc-dashboard-mode)
      (bc-dashboard-render))))

(defun bc-dashboard ()
  "Open the Basecamp Global Dashboard."
  (interactive)
  (let ((buf (get-buffer-create bc-dashboard-buffer-name)))
    (with-current-buffer buf
      (bc-dashboard-mode)
      (bc-dashboard-render))
    (switch-to-buffer buf)))

;;; RPC Handlers for CLI (`emacsclient`)

(defun bc-rpc-status (&optional root-or-dir json-p)
  "RPC: Get Basecamp status overview for ROOT-OR-DIR."
  (let* ((root (bc-current-project-root root-or-dir))
         (metrics (and root (bc--project-metrics root)))
         (tasks (and root (bc--extract-tasks root "TODO|IN-PROGRESS")))
         (messages (and root (bc--extract-messages root)))
         (activities (and root (bc--extract-activities root 5)))
         (res `((project . ,(and root (file-name-nondirectory (directory-file-name root))))
                (root . ,root)
                (metrics . ,metrics)
                (active_tasks . ,tasks)
                (recent_messages . ,messages)
                (recent_activities . ,activities))))
    (if json-p
        (json-encode res)
      res)))

(defun bc-rpc-tasks-list (&optional root-or-dir status-filter json-p)
  "RPC: List tasks for ROOT-OR-DIR with optional STATUS-FILTER."
  (let* ((root (bc-current-project-root root-or-dir))
         (tasks (bc--extract-tasks root status-filter)))
    (if json-p
        (json-encode tasks)
      tasks)))

(defun bc-rpc-tasks-add (title &optional priority tags body root-or-dir)
  "RPC: Add a task to ROOT-OR-DIR."
  (bc-add-task title priority tags body root-or-dir)
  t)

(defun bc-rpc-tasks-update (task-id-or-title status &optional note root-or-dir)
  "RPC: Update status of a task matching TASK-ID-OR-TITLE in ROOT-OR-DIR."
  (let* ((root (bc-current-project-root root-or-dir))
         (tasks-path (expand-file-name bc-tasks-file root))
         (updated nil))
    (unless (file-exists-p tasks-path)
      (user-error "Tasks file %s does not exist" tasks-path))
    (with-current-buffer (find-file-noselect tasks-path)
      (goto-char (point-min))
      (while (and (not updated)
                  (re-search-forward "^\\*+ \\(TODO\\|IN-PROGRESS\\|WAITING\\|DONE\\|CANCELED\\) \\(.*\\)$" nil t))
        (let ((cur-title (match-string 2)))
          (when (string-match-p (regexp-quote task-id-or-title) cur-title)
            (org-todo status)
            (when (and note (not (string-empty-p note)))
              (end-of-line)
              (insert (format "\n  - Note [%s]: %s" (format-time-string "%Y-%m-%d %H:%M") note)))
            (setq updated t))))
      (if updated
          (progn
            (save-buffer)
            (bc-refresh)
            t)
        (error "Task matching '%s' not found" task-id-or-title)))))

(defun bc-rpc-checkin (summary &optional body agent-name root-or-dir)
  "RPC: Log a check-in."
  (bc-checkin summary body agent-name root-or-dir)
  t)

(defun bc-rpc-messages-list (&optional root-or-dir category-filter json-p)
  "RPC: List messages."
  (let* ((root (bc-current-project-root root-or-dir))
         (msgs (bc--extract-messages root category-filter)))
    (if json-p
        (json-encode msgs)
      msgs)))

(defun bc-rpc-messages-add (title category body &optional root-or-dir)
  "RPC: Post a message."
  (bc-post-message title category body root-or-dir)
  t)

(defun bc-rpc-specs-read (&optional spec-name root-or-dir)
  "RPC: Read specification file content."
  (let* ((root (bc-current-project-root root-or-dir))
         (target (if (and spec-name (not (string-empty-p spec-name)))
                     (expand-file-name spec-name (expand-file-name bc-docs-directory root))
                   (expand-file-name bc-specs-file root))))
    (if (file-exists-p target)
        (with-temp-buffer
          (insert-file-contents target)
          (buffer-string))
      (error "Spec file %s not found" target))))

(defun bc-rpc-refresh (&optional root-or-dir)
  "RPC: Refresh Basecamp."
  (let ((default-directory (or (bc-current-project-root root-or-dir) default-directory)))
    (bc-refresh)
    t))

;;; Keymap Setup

(defvar bc-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'bc-dashboard)
    (define-key map (kbd "h") #'bc-open-hq)
    (define-key map (kbd "t") #'bc-add-task)
    (define-key map (kbd "i") #'bc-checkin)
    (define-key map (kbd "m") #'bc-post-message)
    (define-key map (kbd "r") #'bc-refresh)
    (define-key map (kbd "a") #'bc-run-agent)
    (define-key map (kbd "+") #'bc-init)
    map)
  "Prefix keymap for Basecamp commands.")

(global-set-key (kbd "C-c b") bc-prefix-map)

(provide 'bc)

;;; bc.el ends here
