;;; github.el --- GitHub API client for Emacs

;;; Commentary:

;; GitHub API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; github/clone-or-pull-repo.

;; If `magit' is installed, this will be used, otherwise we shell out to git,
;; so obviously, that needs to be installed on your system.

;; You can also clone or pull all repositories in one fail swoop with
;; github/clone-or-pull-all.  This will not use `magit', unless called with
;; prefix argument.

;; github/retriever allows arbitrary querying of endpoints.  If
;; `json-navigator' is installed, the JSON results will be opened in a fancy
;; tree hiearchy browser.  If not, you can view the JSON results pretty printed
;; in a dedicated buffer.

;; You will be asked to enter your Personal Access Token the first time you
;; run a command.

;; Generate a this here: https://github.com/settings/tokens.

;; By default these tokens will be stored in $HOME/.emacs.d/.gitlab.token and
;; $HOME/.emacs.d/.github.token, however these locations can be customised with
;; `customize' or `setq'.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'lazygit)

(defcustom github/token-file (concat user-emacs-directory ".github.token")
  "File to store GitHub Personal Access Token in."
  :group 'lazygit/tokens
  :type 'file)

(defun github/install-token (token)
  "Prompt for GitHub TOKEN and write it to `github/token-file'."
  (interactive "sEnter your GitHub Personal Access Token: ")
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
  (interactive "sEnter a GitHub API endpoint: ")
  (url/view-retrieved-json (concat "https://api.github.com/"
                                   endpoint
                                   "?per_page=100&page=1")
			   "*lazygithub*"
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

(defalias 'gh/api 'github/retriever)
(defalias 'gh/all 'github/clone-or-pull-all)
(defalias 'gh/repo 'github/clone-or-pull-repo)

(provide 'github)
;;; github.el ends here
