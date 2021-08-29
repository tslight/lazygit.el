;;; lazygit.el --- Git Forge Clients -*- lexical-binding: t; -*-

;;; Commentary:

;; So far only GitHub & GitLab are supported.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; URL: https://github.com/tslight/lazygit.el
;; Version: 0
;; Package-Requires: ((emacs "24.4"))

;;; Code:
(require 'auth-source)
(require 'json)
(require 'url)
(require 'vc)

;;;###autoload
(defgroup lazygit nil "LazyGit configuration." :group 'convenience)

;;;###autoload
(defcustom lazygit-directory (expand-file-name "~")
  "Path to start looking for git repositories recursively under."
  :group 'lazygit
  :type 'directory)

;;;###autoload
(defcustom lazygit-maxdepth 12
  "Maximum depth to descent when searching for git repositories."
  :group 'lazygit
  :type 'integer)

;;;###autoload
(defun lazygit-secret-from-authinfo (host)
  "Retrieve API token for HOST from `authinfo' file.
If there is nothing found for HOST prompt the user to enter one."
  (let* ((auth-source-creation-prompts '((secret . "Enter %h API token: ")))
         (save-function (plist-get
                         (car (auth-source-search
                               :max 1
                               :host host
                               :user "api-token"
                               :create t))
                         :save-function))
         (secret (plist-get
                  (car (auth-source-search
                        :max 1
                        :host host
                        :user "api-token"
                        :create t))
                  :secret)))
    (if (functionp save-function)
        (funcall save-function))
    (if (functionp secret)
        (funcall secret))))

;;;###autoload
(defun lazygit-delete-buffer ()
  "Delete *lazygit* buffer, if it exists."
  (when (get-buffer "*lazygit*")
    (kill-buffer "*lazygit*")))

;;;###autoload
(defun lazygit-filter-keys (list keys)
  "Filter a LIST of association lists by a list of KEYS."
  (mapcar (lambda (al) (mapcar (lambda (key) (assoc key al)) keys)) list))

;;;###autoload
(defun lazygit-get-assoc-list (list key value)
  "Return an alist from LIST of association lists that has KEY VALUE pair."
  (car (remove nil (mapcar
                    (lambda (e) (if (equal value (cdr (assoc key e))) e))
                    list))))

;;;###autoload
(defun lazygit-flatten-json (json-arrays)
  "Turn a list of JSON-ARRAYS into a single JSON array."
  (let ((json-array-type 'list))
    (json-encode-list (mapcan
                       #'json-read-from-string
                       json-arrays))))

;;;###autoload
(defun lazygit-link-to-next-page ()
  "Return a link to the next page of results.
Assumes typical key-set based pagination return headers."
  (goto-char (point-min))
  (let ((begin (re-search-forward "Link.*: <" nil t))
        (end (re-search-forward ">; rel=\"next\"" nil t)))
    (if (and begin end)
        (buffer-substring-no-properties begin (- end 13))
      nil)))

;;;###autoload
(defun lazygit-retrieve-paginated-bodies (url &optional headers items)
  "Return all ITEMS from URL, with optional HEADERS.
Supports key-based pagination - i.e) if returned headers have a
link to the next page."
  (with-current-buffer
      (let ((url-request-method "GET")
            (url-request-extra-headers headers))
        (url-retrieve-synchronously url))
    (let ((next-page (lazygit-link-to-next-page)))
      (delete-region (point-min) url-http-end-of-headers)
      (let* ((retrieved-items (cons (buffer-string) items)))
        (if next-page
            (lazygit-retrieve-paginated-bodies next-page headers retrieved-items)
          retrieved-items)))))

;;;###autoload
(defun lazygit-parse-retrieved-json (url &optional headers)
  "Return association lists from JSON bodies retrieved from URL.
Optional HEADERS can be specified."
  (let* ((json (lazygit-retrieve-paginated-bodies url headers))
         (json-array-type 'list)
         (parsed-json (json-read-from-string (lazygit-flatten-json json))))
    parsed-json))

;;;###autoload
(defun lazygit-view-retrieved-json (url buffer &optional headers)
  "View the JSON retrieved from URL, with optional HEADERS, in BUFFER.
Results will be pretty printed in a buffer."
  (let ((items (lazygit-retrieve-paginated-bodies url headers)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert (lazygit-flatten-json items))
    (json-pretty-print-buffer)
    (goto-char (point-min))
    (if (fboundp 'json-mode) (json-mode))))

;;;###autoload
(defun lazygit-get-values (url keys &optional headers)
  "Retrieve values from KEYS from a list of URL JSON objects."
  (let ((response (lazygit-parse-retrieved-json url headers)))
    (lazygit-filter-keys response keys)))

;;;###autoload
(defun lazygit-repo-p (directory)
  "Return non-nil if there is a git repository in DIRECTORY."
  (and (file-directory-p (concat directory "/.git"))
       (file-directory-p (concat directory "/.git/info"))
       (file-directory-p (concat directory "/.git/objects"))
       (file-directory-p (concat directory "/.git/refs"))
       (file-regular-p (concat directory "/.git/HEAD"))))

;;;###autoload
(defun lazygit-process-sentinel (process event)
  "Write output of PROCESS with EVENT, after filtering."
  (when (and (memq (process-status process) '(exit signal))
             (not (equal event "finished\n")))
    (message (string-trim (concat (process-name process) " " event)))))

;;;###autoload
(defun lazygit-find-file-button (button)
  "Find a file in BUTTON."
  (find-file (buffer-substring (button-start button) (button-end button))))
;; https://superuser.com/a/331896
(define-button-type 'lazygit-find-file-button
  'follow-link t
  'action #'lazygit-find-file-button)

;;;###autoload
(defun lazygit-process-filter (proc string)
  "Filter the STRING produced by PROC."
  (when (and string
             (not (string-match-p ".*already up to date.*" (downcase string)))
             (not (string-match-p "^remote\\:.*" (downcase string)))
             (not (string-match-p "^warning.*permanently added.*known hosts" (downcase string)))
             (not (string-match-p ".*[0-9]+%.*" (downcase string)))
             (buffer-live-p (process-buffer proc)))
    (display-buffer (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (vc-compilation-mode 'git) (read-only-mode -1) (button-mode)
      (face-remap-add-relative 'button
                               :weight 'semibold
                               :background "black"
                               :foreground "yellow"
                               :underline t)
      (let* ((moving (= (point) (process-mark proc)))
             (timestamp (format-time-string "%T "))
             (directory (process-name proc))
             (header (concat timestamp directory))
             (buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
        (save-excursion
          (unless (string-match-p (concat "^" header "$") buffer-contents)
            (goto-char (process-mark proc))
            (insert (propertize timestamp
                                'font-lock-face '(:foreground "cyan" :weight semibold)))
            (insert-text-button directory :type 'lazygit-find-file-button)
            (insert "\n")
            (set-marker (process-mark proc) (point)))
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (replace-regexp-in-string (string 13) "\n" string))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      (read-only-mode 1))))

;;;###autoload
(defun lazygit-async-shell-command (command directory)
  "Run COMMAND with name as DIRECTORY, asynchronously."
  (let ((process (start-process-shell-command
                  (abbreviate-file-name directory)
                  "*lazygit*"
                  command)))
    (set-process-filter process 'lazygit-process-filter)
    (set-process-sentinel process 'lazygit-process-sentinel)))

;;;###autoload
(defun lazygit-command (directory command)
  "Run a git COMMAND in DIRECTORY."
  (lazygit-async-shell-command (concat "git -C " directory " " command) directory))

;;;###autoload
(defun lazygit-clone-or-pull (directory url)
  "Clone or pull a git repository from URL to DIRECTORY."
  (if (lazygit-repo-p directory)
      (lazygit-command directory "pull --stat")
    (lazygit-async-shell-command (concat "git clone " url " " directory) directory)))

;;;###autoload
(defun lazygit-clone-or-pull-repo (repos path name url directory)
  "Prompt for a repository from REPOS, then clone that repository to DIRECTORY.
Using PATH, NAME & URL."
  (let* ((paths (mapcar (lambda (r) (cdr (assoc path r))) repos))
         (choice (completing-read "Repo to clone: " paths))
         (repo (lazygit-get-assoc-list repos path choice))
         (cloneurl (cdr (assoc url repo)))
         (reponame (cdr (assoc name repo)))
         (pathname (cdr (assoc path repo)))
         (dest (concat directory "/" pathname)))
    (lazygit-clone-or-pull dest cloneurl)))

;;;###autoload
(defun lazygit-clone-or-pull-batch (repos directory pathkey urlkey)
  "Batch pull or clone REPOS to DIRECTORY using PATHKEY and URLKEY."
  (lazygit-delete-buffer)
  (mapc (lambda (r)
          (make-directory (concat directory "/" (cdr (assoc pathkey r))) t)
          (lazygit-clone-or-pull
           (concat directory "/" (cdr (assoc pathkey r)))
           (cdr (assoc urlkey r))))
        repos))

;;;###autoload
(defun lazygit-repos-recursive (directory maxdepth)
  "List git repos in under DIRECTORY recursively to MAXDEPTH."
  (let* ((git-repos '())
         (current-directory-list
          (directory-files directory t directory-files-no-dot-files-regexp)))
    ;; while we are in the current directory
    (if (lazygit-repo-p directory)
        (setq git-repos (cons (file-truename (expand-file-name directory)) git-repos)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond ((and (file-directory-p f)
                    (file-readable-p f)
                    (> maxdepth 0)
                    (not (lazygit-repo-p f)))
               (setq git-repos
                     (append git-repos
                             (lazygit-repos-recursive f (- maxdepth 1)))))
              ((lazygit-repo-p f)
               (setq git-repos (cons
                                (file-truename (expand-file-name f)) git-repos))))
        (setq current-directory-list (cdr current-directory-list))))
    (delete-dups git-repos)))

;;;###autoload
(defun lazygit-command-batch (directory command maxdepth)
  "Run git COMMAND in all sub-directories (to MAXDEPTH) of DIRECTORY."
  (lazygit-delete-buffer)
  (mapc (lambda (directory)
          (lazygit-command directory command))
        (lazygit-repos-recursive directory maxdepth))
  (message "Ran git %s on all repos %d directories deep under %s"
           command maxdepth directory))

;;;###autoload
(defun lazygit-pull-all (arg &optional directory)
  "Git pull on all projects in DIRECTORY.
If `prefix' only look for git repos ARG deep.
Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-command-batch (if directory directory lazygit-directory)
                         "pull --stat"
                         (if (> arg 1) arg lazygit-maxdepth)))

;;;###autoload
(defun lazygit-status-all (arg &optional directory)
  "Git status on all projects in DIRECTORY.
If `prefix' only look for git repos ARG deep.
Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-command-batch (if directory directory lazygit-directory)
                         "status --porcelain"
                         (if (> arg 1) arg lazygit-maxdepth)))

;;;###autoload
(defvar lazygit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'lazygit-status-all)
    (define-key map (kbd "p") 'lazygit-pull-all)
    map)
  "Keymap for `lazygit' commands.")
(fset 'lazygit-map lazygit-map)

(provide 'lazygit)
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; lazygit.el ends here
