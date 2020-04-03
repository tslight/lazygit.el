;;; github.el --- GitHub API client for Emacs

;;; Commentary:

;; GitHub API client for Emacs.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'lazygit)

(defcustom github/token-file (concat user-emacs-directory ".github.key")
  "File to store GitHub API key in."
  :group 'lazygit/tokens
  :type 'file)

(defun github/install-token (token)
  "Prompt for GitHub TOKEN and write it to `github/token-file'."
  (interactive "sEnter your GitHub API key: ")
  (write-region token nil github/token-file)
  token)

(defun github/token? ()
  "Check it `github/token-file' exists and is non-empty."
  (if (and (file-readable-p github/token-file)
	   (file-regular-p github/token-file))
      (read-file github/token-file)
    (call-interactively #'github/install-token)))

(defun github/retriever (endpoint)
  "Retrieve resources from GitHub ENDPOINT."
  (interactive "sEnter an endpoint: ")
  (url/view-retrieved-json (concat "https://api.github.com/"
                                   endpoint
                                   "?per_page=100&page=1")
                           ;; https://stackoverflow.com/a/24188208
                           `(("Authorization" . ,(concat "token " (github/token?))))))

(defun github/get-values (endpoint keys)
  "Retrieve values from KEYS of GitHub ENDPOINT json resources."
  (url/get-values (concat "https://api.github.com/"
                          endpoint
                          "?per_page=100&page=1")
                  keys
                  ;; https://stackoverflow.com/a/24188208
                  `(("Authorization" . ,(concat "token " (github/token?))))))

(defun github/clone-or-pull-repo (directory)
  "Clone or pull repo to DIRECTORY."
  (interactive "DDirectory to clone to: ")
  (git/clone-or-pull-repo (github/get-values "user/repos" (list 'full_name
								'name
								'ssh_url))
                          'full_name 'name 'ssh_url directory))

(defun github/clone-or-pull-all (directory)
  "Clone or pull ALL repos to DIRECTORY."
  (interactive "DDirectory to clone to: ")
  (let ((repos (github/get-values "user/repos" (list 'full_name 'ssh_url))))
    (mapc (lambda (r)
            (make-directory (concat directory "/" (cdr (assoc 'full_name r))) t)
            (git/clone-or-pull
             (concat directory "/" (cdr (assoc 'full_name r)))
             (cdr (assoc 'ssh_url r))))
          repos)))

(defalias 'gh/all 'github/clone-or-pull-all)
(defalias 'gh/repo 'github/clone-or-pull-repo)

(provide 'github)
;;; github.el ends here
