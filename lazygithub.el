;; -*- lexical-binding: t; -*-
;;; lazygithub.el --- GitHub API client for Emacs

;;; Commentary:

;; GitHub API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; lazygithub/clone-or-pull-repo.

;; If `magit' is installed, this will be used, otherwise we shell out to git,
;; so obviously, that needs to be installed on your system.

;; You can also clone or pull all repositories in one fail swoop with
;; lazygithub/clone-or-pull-all.  This will not use `magit', unless called with
;; prefix argument.

;; lazygithub/retriever allows arbitrary querying of endpoints.  If
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

(require 'lazygit)

(defcustom lazygithub-token-file (concat user-emacs-directory ".github.token")
  "File to store GitHub Personal Access Token in."
  :group 'lazygit-tokens
  :type 'file)

(defun lazygithub-install-token (token)
  "Prompt for GitHub TOKEN and write it to `lazygithub-token-file'."
  (interactive "sEnter your GitHub Personal Access Token: ")
  (write-region token nil lazygithub-token-file)
  token)

(defun lazygithub-token-p ()
  "Check it `lazygithub-token-file' exists and is non-empty."
  (if (and (file-readable-p lazygithub-token-file)
	   (file-regular-p lazygithub-token-file))
      (lazygit-read-file lazygithub-token-file)
    (call-interactively #'lazygithub-install-token)))

(defun lazygithub-retriever (endpoint)
  "Retrieve resources from GitHub ENDPOINT."
  (interactive "sEnter a GitHub API endpoint: ")
  (lazygit-view-retrieved-json (concat "https://api.github.com/"
                                       endpoint
                                       "?per_page=100&page=1")
			       "*lazygithub*"
                               `(("Authorization" . ,(concat "token "
							     (lazygithub-token-p))))))

(defun lazygithub-get-values (endpoint keys)
  "Retrieve values from KEYS of GitHub ENDPOINT json resources."
  (lazygit-get-values (concat "https://api.github.com/"
                              endpoint
                              "?per_page=100&page=1")
                      keys
                      `(("Authorization" . ,(concat "token " (lazygithub-token-p))))))

(defun lazygithub-clone-or-pull-repo (directory)
  "Clone or pull repo to DIRECTORY."
  (interactive "DDirectory to clone GitHub repo to: ")
  (lazygit-clone-or-pull-repo (lazygithub-get-values "user/repos" (list 'full_name
									'name
									'ssh_url))
                              'full_name 'name 'ssh_url directory))

(defun lazygithub-clone-or-pull-all (directory)
  "Clone or pull ALL GitHub repos to DIRECTORY."
  (interactive "DDirectory to clone ALL GitHub repos to: ")
  (let ((repos (lazygithub-get-values "user/repos" (list 'full_name 'ssh_url))))
    (lazygit-clone-or-pull-batch repos directory 'full_name 'ssh_url)))

(defalias 'gh/api 'lazygithub-retriever)
(defalias 'gh/all 'lazygithub-clone-or-pull-all)
(defalias 'gh/repo 'lazygithub-clone-or-pull-repo)

(provide 'lazygithub)
;;; lazygithub.el ends here
