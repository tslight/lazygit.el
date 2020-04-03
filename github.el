;;; github.el --- GitHub API client for Emacs

;;; Commentary:

;; GitHub API client for Emacs.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'lazygit)

(defun github/retriever (endpoint)
  "Retrieve resources from GitHub ENDPOINT."
  (interactive "sEnter an endpoint: ")
  (url/view-retrieved-json (concat "https://api.github.com/"
                                   endpoint
                                   "?per_page=100&page=1")
                           ;; https://stackoverflow.com/a/24188208
                           `(("Authorization" . ,(concat "token " github/api-key)))))

(defun github/get-values (endpoint keys)
  "Retrieve values from KEYS of GitHub ENDPOINT json resources."
  (url/get-values (concat "https://api.github.com/"
                          endpoint
                          "?per_page=100&page=1")
                  keys
                  ;; https://stackoverflow.com/a/24188208
                  `(("Authorization" . ,(concat "token " github/api-key)))))

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
