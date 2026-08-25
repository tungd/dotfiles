;;; bc-tests.el --- ERT tests for Basecamp mode -*- lexical-binding: t; -*-

;; Author: Tung Dao <me@tungdao.com>
;; Keywords: tests, project, org, agents

;;; Commentary:
;; ERT test suite for `bc.el` (Basecamp integrated project environment).

;;; Code:

(require 'ert)
(require 'org)

(setq load-prefer-newer t)

(add-to-list 'load-path
             (expand-file-name "user-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'bc)

(defmacro bc-test-with-temp-project (&rest body)
  "Create a temporary directory, initialize Basecamp, and evaluate BODY with it as root."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "bc-test-project-" t))
          (default-directory (file-name-as-directory temp-dir))
          (bc-projects-directories (list (file-name-directory (directory-file-name temp-dir)))))
     (unwind-protect
         (progn
           (bc-init temp-dir)
           ,@body)
       (delete-directory temp-dir t))))

;;; 1. Scaffolding & Root Detection Tests

(ert-deftest bc-test-init-creates-cards-and-hq ()
  "Test that `bc-init' creates all necessary Basecamp files."
  (bc-test-with-temp-project
    (should (file-exists-p (expand-file-name "PROJECT.org" default-directory)))
    (should (file-exists-p (expand-file-name "docs/TASKS.org" default-directory)))
    (should (file-exists-p (expand-file-name "docs/ACTIVITIES.org" default-directory)))
    (should (file-exists-p (expand-file-name "docs/MESSAGES.org" default-directory)))
    (should (file-exists-p (expand-file-name "docs/SPECS.org" default-directory)))
    (should (bc-project-p default-directory))))

(ert-deftest bc-test-root-detection-from-subfolder ()
  "Test that `bc-current-project-root' locates root from a nested subfolder."
  (bc-test-with-temp-project
    (let ((nested (expand-file-name "src/deep/nested" default-directory)))
      (make-directory nested t)
      (should (equal (file-truename (bc-current-project-root nested))
                     (file-truename default-directory))))))

;;; 2. Task Management & Extraction Tests

(ert-deftest bc-test-add-and-extract-tasks ()
  "Test adding tasks and extracting task plists."
  (bc-test-with-temp-project
    (bc-add-task "Refactor auth controller" "A" "auth:refactor" "Details about auth refactor")
    (bc-add-task "Write integration tests" "B" "test")
    (let ((tasks (bc--extract-tasks default-directory)))
      (should (>= (length tasks) 3))
      (let ((auth-task (seq-find (lambda (x) (string-match-p "Refactor auth controller" (plist-get x :title))) tasks)))
        (should auth-task)
        (should (equal (plist-get auth-task :status) "TODO"))
        (should (equal (plist-get auth-task :priority) "A"))
        (should (member "auth" (plist-get auth-task :tags)))))))

(ert-deftest bc-test-update-task-status ()
  "Test updating task status and appending progress notes via RPC."
  (bc-test-with-temp-project
    (bc-add-task "Design schema" "A")
    (bc-rpc-tasks-update "Design schema" "IN-PROGRESS" "Started schema draft" default-directory)
    (let* ((tasks (bc--extract-tasks default-directory "IN-PROGRESS"))
           (task (seq-find (lambda (x) (string-match-p "Design schema" (plist-get x :title))) tasks)))
      (should task)
      (should (equal (plist-get task :status) "IN-PROGRESS")))
    (bc-rpc-tasks-update "Design schema" "DONE" "Schema finalized and tested" default-directory)
    (let* ((done-tasks (bc--extract-tasks default-directory "DONE"))
           (done-task (seq-find (lambda (x) (string-match-p "Design schema" (plist-get x :title))) done-tasks)))
      (should done-task)
      (should (equal (plist-get done-task :status) "DONE")))))

;;; 3. Check-ins & Activity Log Tests

(ert-deftest bc-test-checkin-logging ()
  "Test logging check-ins into ACTIVITIES.org."
  (bc-test-with-temp-project
    (bc-checkin "Completed parser refactor" "Added 12 unit tests" "Agent" default-directory)
    (let ((acts (bc--extract-activities default-directory 5)))
      (should (>= (length acts) 2))
      (let ((latest (car acts)))
        (should (string-match-p "Completed parser refactor" (plist-get latest :title)))
        (should (equal (plist-get latest :author) "Agent"))))))

;;; 4. Message Board Tests

(ert-deftest bc-test-message-board-posting ()
  "Test posting pitches and announcements to MESSAGES.org."
  (bc-test-with-temp-project
    (bc-post-message "Async Worker Pitch" "pitch" "Proposal for background task workers" default-directory)
    (let ((msgs (bc--extract-messages default-directory "pitch")))
      (should (>= (length msgs) 1))
      (let ((pitch (car msgs)))
        (should (equal (plist-get pitch :title) "Async Worker Pitch"))
        (should (member "pitch" (plist-get pitch :tags)))))))

;;; 5. Org Dynamic Blocks Tests

(ert-deftest bc-test-dynamic-blocks-refresh ()
  "Test that `bc-refresh' updates dynamic blocks in PROJECT.org."
  (bc-test-with-temp-project
    (bc-add-task "Feature X" "A" "feat")
    (bc-checkin "Checkin Y" nil "Tung" default-directory)
    (bc-refresh)
    (let ((hq-file (expand-file-name "PROJECT.org" default-directory)))
      (with-temp-buffer
        (insert-file-contents hq-file)
        (let ((content (buffer-string)))
          (should (string-match-p "Feature X" content))
          (should (string-match-p "Checkin Y" content))
          (should (string-match-p "docs/TASKS.org" content)))))))

;;; 6. Global Dashboard Rendering Tests

(ert-deftest bc-test-dashboard-renders-project-card ()
  "Test that `bc-dashboard-render' generates project cards and metrics."
  (bc-test-with-temp-project
    (let ((buf (get-buffer-create "*Basecamp-Test*")))
      (unwind-protect
          (with-current-buffer buf
            (bc-dashboard-mode)
            (bc-dashboard-render)
            (let ((content (buffer-string)))
              (should (string-match-p "BASECAMP HQ" content))
              (should (string-match-p (file-name-nondirectory (directory-file-name default-directory)) content))
              (should (string-match-p "TODO" content))
              (should (string-match-p "RECENT ACTIVITY & CHECK-INS" content))))
        (kill-buffer buf)))))

;;; 7. RPC Serialization Tests

(ert-deftest bc-test-rpc-status-json ()
  "Test that `bc-rpc-status' produces valid JSON output."
  (bc-test-with-temp-project
    (let* ((json-str (bc-rpc-status default-directory t))
           (parsed (json-read-from-string json-str)))
      (should parsed)
      (should (alist-get 'project parsed))
      (should (alist-get 'root parsed))
      (should (alist-get 'metrics parsed)))))

(provide 'bc-tests)

;;; bc-tests.el ends here
