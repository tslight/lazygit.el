;;; lazygit.el --- Git hosting API clients for Emacs

;;; Commentary:

;; So far only GitHub & GitLab are supported.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'json)
(require 'package)
(require 'url)
(require 'utils)

(defgroup lazygit/tokens nil
  "API keys."
  :group 'convenience)

(defun url/link-to-next-page ()
  "Return a link to the next page of results.
Assumes typical keyset based pagination return headers."
  (goto-char (point-min))
  (let ((begin (re-search-forward "Link.*: <" nil t))
        (end (re-search-forward ">; rel=\"next\"" nil t)))
    (if (and begin end)
        (buffer-substring-no-properties begin (- end 13))
      nil)))

(defun url/retrieve-paginated-bodies (url &optional headers items)
  "Return all ITEMS from URL, with optional HEADERS.
Supports key-based pagination - ie) if returned headers have a
link to the next page."
  (set-buffer
   (let ((url-request-method "GET")
         (url-request-extra-headers headers))
     (url-retrieve-synchronously url)))
  (let ((next-page (url/link-to-next-page)))
    (delete-region (point-min) url-http-end-of-headers)
    (let* ((retrieved-items (cons (buffer-string) items)))
      (if next-page
          (url/retrieve-paginated-bodies next-page headers retrieved-items)
        retrieved-items))))

(defun url/parse-retrieved-json (url &optional headers)
  "Return alists from JSON bodies retrieved from URL.
Optional HEADERS can be specifified."
  (let* ((json (url/retrieve-paginated-bodies url headers))
         (json-array-type 'list)
         (parsed-json (json-read-from-string (flatten-json json))))
    parsed-json))

(defun url/view-retrieved-json (url buffer &optional headers)
  "View the json retreived from URL, with optional HEADERS, in BUFFER.
Results will be pretty printed in a buffer, and if
`json-navigator' is installed will be viewed opened in that."
  (let ((items (url/retrieve-paginated-bodies url headers)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert (flatten-json items))
    (json-pretty-print-buffer)
    (goto-char (point-min))
    (json-mode)
    (if (package-installed-p 'json-navigator)
        (progn (json-navigator-navigate-after-point)
               (execute-kbd-macro (kbd "<return>"))))))

(defun url/get-values (url keys &optional headers)
  "Retrieve values from KEYS from a list of URL json objects."
  (let ((response (url/parse-retrieved-json url headers)))
    (filter-keys response keys)))

(defun git/repo? (directory)
  "Return non-nil if there is a git repo in DIRECTORY."
  (and
   (file-directory-p (concat directory "/.git"))
   (file-directory-p (concat directory "/.git/info"))
   (file-directory-p (concat directory "/.git/objects"))
   (file-directory-p (concat directory "/.git/refs"))
   (file-regular-p (concat directory "/.git/HEAD"))))

(defun git/clone-or-pull (directory url &optional with-magit)
  "Clone or pull a git repo from URL to DIRECTORY.
If WITH-MAGIT is true and `magit' is installed, use that instead
of shelling out to git."
  (if (git/repo? directory)
      (if (and with-magit (package-installed-p 'magit))
          (progn
            (message
             (concat directory ": "
                     (string-trim
                      (shell-command-to-string
                       (concat "git -C " directory " pull")))))
            (magit-status-setup-buffer directory))
	(message-async-shell-command
	 (concat "git -C " directory " pull --quiet")))
    (progn
      (if (and with-magit (package-installed-p 'magit))
	  (magit-clone-regular url directory nil)
	(message-async-shell-command
	 (concat "git clone --quiet " url " " directory))))))

(defun git/clone-or-pull-repo (repos path name url directory)
  "Prompt for a repo from REPOS, then clone that repo to DIRECTORY.
Using PATH, NAME & URL."
  (let* ((paths (mapcar (lambda (r) (cdr (assoc path r))) repos))
         (choice (completing-read "Repo to clone: " paths))
         (repo (get-assoc-list repos path choice))
         (cloneurl (cdr (assoc url repo)))
         (reponame (cdr (assoc name repo)))
         (dest (concat directory reponame)))
    (git/clone-or-pull dest cloneurl t)))

(defun git/clone-or-pull-batch (repos directory pathkey urlkey)
  "Batch pull or clone REPOS to DIRECTORY using PATHKEY and URLKEY."
  (mapc (lambda (r)
          (make-directory (concat directory "/"
				  (cdr (assoc pathkey r)))
			  t)
          (git/clone-or-pull
           (concat directory "/" (cdr (assoc pathkey r)))
           (cdr (assoc urlkey r))))
        repos))

(provide 'lazygit)
;;; lazygit.el ends here
