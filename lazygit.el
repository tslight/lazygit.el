;;; lazygit.el --- Git Forge Clients -*- lexical-binding: t; -*-

;;; Commentary:

;; So far only GitHub & GitLab are supported.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; URL: https://github.com/tslight/lazygit.el
;; Package-Requires: ((Emacs "27.1"))

;;; Code:
(require 'auth-source)
(require 'json)
(require 'url)

(defgroup lazygit nil "LazyGit configuration." :group 'convenience)

(defun lazygit-secret-from-authinfo (host)
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
(defun lazygit-message-sentinel-output (process msg)
  "Write output of PROCESS with MSG."
  (when (memq (process-status process) '(exit signal))
    (message (string-trim (concat (process-name process) " " msg)))
    (when (get-buffer "*lazygit*")
      (when (> (buffer-size (get-buffer "*lazygit*")) 0)
        (display-buffer (get-buffer "*lazygit*"))))))

;;;###autoload
(defun lazygit-message-async-shell-command (command)
  "Run COMMAND asynchronously and output results to `minibuffer'."
  (set-process-sentinel (start-process-shell-command command "*lazygit*" command)
                        'lazygit-message-sentinel-output))

;;;###autoload
(defun lazygit-command (directory command)
  "Run a git COMMAND in DIRECTORY."
  (lazygit-message-async-shell-command
   (concat "git -C " directory " " command " --porcelain")))

;;;###autoload
(defun lazygit-clone-or-pull (directory url)
  "Clone or pull a git repository from URL to DIRECTORY."
  (if (lazygit-repo-p directory)
      (lazygit-command directory "pull")
    (lazygit-message-async-shell-command (concat "git clone " url " " directory))))

;;;###autoload
(defun lazygit-clone-or-pull-repo (repos path name url directory)
  "Prompt for a repository from REPOS, then clone that repository to DIRECTORY.
Using PATH, NAME & URL."
  (let* ((paths (mapcar (lambda (r) (cdr (assoc path r))) repos))
         (choice (completing-read "Repo to clone: " paths))
         (repo (lazygit-get-assoc-list repos path choice))
         (cloneurl (cdr (assoc url repo)))
         (reponame (cdr (assoc name repo)))
         (dest (concat directory "/" reponame)))
    (lazygit-clone-or-pull dest cloneurl)))

;;;###autoload
(defun lazygit-clone-or-pull-batch (repos directory pathkey urlkey)
  "Batch pull or clone REPOS to DIRECTORY using PATHKEY and URLKEY."
  (mapc (lambda (r)
          (make-directory (concat directory "/" (cdr (assoc pathkey r))) t)
          (lazygit-clone-or-pull
           (concat directory "/" (cdr (assoc pathkey r)))
           (cdr (assoc urlkey r))))
        repos))

;;;###autoload
(defun lazygit-command-batch (repos directory pathkey command)
  "Run COMMAND pointing at DIRECTORY/PATHKEY."
  (mapc (lambda (r)
          (lazygit-command
           (concat directory "/" (cdr (assoc pathkey r))) command))
        repos))

(provide 'lazygit)
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; lazygit.el ends here
