;; -*- lexical-binding: t; -*-
;;; lazygit.el --- Git hosting API clients for Emacs

;;; Commentary:

;; So far only GitHub & GitLab are supported.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'url)

(defgroup lazygit-tokens nil
  "API keys."
  :group 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Utility Functions                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lazygit-read-file (file)
  "Return FILE content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun lazygit-filter-keys (list keys)
  "Filter a LIST of association lists by a list of KEYS."
  (mapcar (lambda (al)
            (mapcar (lambda (key)
                      (assoc key al))
                    keys))
          list))

(defun lazygit-get-assoc-list (list key value)
  "Return an alist from LIST of alists that has KEY VALUE pair."
  (car (cl-remove-if nil (mapcar
                          (lambda (e) (if (equal value (cdr (assoc key e))) e))
                          list))))

(defun lazygit-flatten-json (items)
  "Turn a list of json arrays (ITEMS) into a single json array."
  (concat "[" (mapconcat
               (lambda (item)
                 (let*
                     ((trimmed (replace-regexp-in-string "^\\[\\]$" "" item))
                      (trimmed (replace-regexp-in-string "^\\[" "" trimmed))
                      (trimmed (if (equal item (car (last items)))
                                   (replace-regexp-in-string "\\]$" "" trimmed)
                                 (replace-regexp-in-string "\\]$" "," trimmed))))
                   trimmed))
               items "")
          "]"))

;; Not using this anymore, but if you'd rather output to a dedicated buffer,
;; change calls to lazygit-message-async-shell-command to this.
(defun lazygit-display-async-shell-command-buffer (command buffer-name &optional erase)
  "Display the results of asynchronous COMMAND in BUFFER-NAME.
If ERASE is true erase the buffer first"
  (let ((buffer (get-buffer-create buffer-name)))
    (if erase (progn (set-buffer buffer) (erase-buffer)))
    (display-buffer buffer)
    (start-process-shell-command command buffer command)))

(defun lazygit-message-sentinel-output (process msg)
  "Write output of PROCESS with MSG."
  (when (memq (process-status process) '(exit signal))
    (message (string-trim (concat (process-name process) " " msg)))))

(defun lazygit-message-async-shell-command (command)
  "Run COMMAND asynchronously and output results to `minibuffer'."
  (set-process-sentinel (start-process-shell-command command nil command)
			#'lazygit-message-sentinel-output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            URL/API Functions                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lazygit-link-to-next-page ()
  "Return a link to the next page of results.
Assumes typical keyset based pagination return headers."
  (goto-char (point-min))
  (let ((begin (re-search-forward "Link.*: <" nil t))
        (end (re-search-forward ">; rel=\"next\"" nil t)))
    (if (and begin end)
        (buffer-substring-no-properties begin (- end 13))
      nil)))

(defun lazygit-retrieve-paginated-bodies (url &optional headers items)
  "Return all ITEMS from URL, with optional HEADERS.
Supports key-based pagination - ie) if returned headers have a
link to the next page."
  (set-buffer
   (let ((url-request-method "GET")
         (url-request-extra-headers headers))
     (url-retrieve-synchronously url)))
  (let ((next-page (lazygit-link-to-next-page)))
    (delete-region (point-min) url-http-end-of-headers)
    (let* ((retrieved-items (cons (buffer-string) items)))
      (if next-page
          (lazygit-retrieve-paginated-bodies next-page headers retrieved-items)
        retrieved-items))))

(defun lazygit-parse-retrieved-json (url &optional headers)
  "Return alists from JSON bodies retrieved from URL.
Optional HEADERS can be specifified."
  (let* ((json (lazygit-retrieve-paginated-bodies url headers))
         (json-array-type 'list)
         (parsed-json (json-read-from-string (lazygit-flatten-json json))))
    parsed-json))

(defun lazygit-view-retrieved-json (url buffer &optional headers)
  "View the json retreived from URL, with optional HEADERS, in BUFFER.
Results will be pretty printed in a buffer, and if
`json-navigator' is installed will be viewed opened in that."
  (let ((items (lazygit-retrieve-paginated-bodies url headers)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert (lazygit-flatten-json items))
    (json-pretty-print-buffer)
    (goto-char (point-min))
    (json-mode)
    (if (package-installed-p 'json-navigator)
        (progn (json-navigator-navigate-after-point)
               (execute-kbd-macro (kbd "<return>"))))))

(defun lazygit-get-values (url keys &optional headers)
  "Retrieve values from KEYS from a list of URL json objects."
  (let ((response (lazygit-parse-retrieved-json url headers)))
    (lazygit-filter-keys response keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Git Functions                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lazygit-repo-p (directory)
  "Return non-nil if there is a git repo in DIRECTORY."
  (and
   (file-directory-p (concat directory "/.git"))
   (file-directory-p (concat directory "/.git/info"))
   (file-directory-p (concat directory "/.git/objects"))
   (file-directory-p (concat directory "/.git/refs"))
   (file-regular-p (concat directory "/.git/HEAD"))))

(defun lazygit-clone-or-pull (directory url &optional with-magit)
  "Clone or pull a git repo from URL to DIRECTORY.
If WITH-MAGIT is true and `magit' is installed, use that instead
of shelling out to git."
  (if (lazygit-repo-p directory)
      (if (and with-magit (package-installed-p 'magit))
          (progn
            (message
             (concat directory ": "
                     (string-trim
                      (shell-command-to-string
                       (concat "git -C " directory " pull")))))
            (magit-status-setup-buffer directory))
	(lazygit-message-async-shell-command
	 (concat "git -C " directory " pull --quiet")))
    (progn
      (if (and with-magit (package-installed-p 'magit))
	  (magit-clone-regular url directory nil)
	(lazygit-message-async-shell-command
	 (concat "git clone --quiet " url " " directory))))))

(defun lazygit-clone-or-pull-repo (repos path name url directory)
  "Prompt for a repo from REPOS, then clone that repo to DIRECTORY.
Using PATH, NAME & URL."
  (let* ((paths (mapcar (lambda (r) (cdr (assoc path r))) repos))
         (choice (completing-read "Repo to clone: " paths))
         (repo (lazygit-get-assoc-list repos path choice))
         (cloneurl (cdr (assoc url repo)))
         (reponame (cdr (assoc name repo)))
         (dest (concat directory "/" reponame)))
    (lazygit-clone-or-pull dest cloneurl t)))

(defun lazygit-clone-or-pull-batch (repos directory pathkey urlkey)
  "Batch pull or clone REPOS to DIRECTORY using PATHKEY and URLKEY."
  (mapc (lambda (r)
          (make-directory (concat directory "/"
				  (cdr (assoc pathkey r)))
			  t)
          (lazygit-clone-or-pull
           (concat directory "/" (cdr (assoc pathkey r)))
           (cdr (assoc urlkey r))))
        repos))

(provide 'lazygit)
;;; lazygit.el ends here
