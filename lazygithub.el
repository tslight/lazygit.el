;;; lazygithub.el --- GitHub API Client -*- lexical-binding: t; -*-

;;; Commentary:

;; GitHub API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; lazygithub/clone-or-pull-repo.

;; You can also clone or pull all repositories in one fail swoop with
;; lazygithub/clone-or-pull-all.

;; lazygithub/retriever allows arbitrary querying of endpoints.  You can view
;; the JSON results pretty printed in a dedicated buffer.

;; You will be asked to enter your Personal Access Token the first time you
;; run a command.

;; Generate this here: https://github.com/settings/tokens.

;; I'm using `auth-source' to retrieve and store this token in ~/.authinfo or
;; ~/.authinfo.gpg

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; URL: https://github.com/tslight/lazygit.el
;; Version: 0
;; Package-Requires: ((emacs "24.4"))

;;; Code:
(require 'lazygit)

;;;###autoload
(defcustom lazygithub-directory (expand-file-name "~/src/github")
  "File to store API Personal Access Tokens in."
  :group 'lazygit
  :type 'directory)

;;;###autoload
(defvar lazygithub-baseurl "https://api.github.com/"
  "GitHub API base URL.")

;;;###autoload
(defvar lazygithub-attr "?per_page=100&page=1"
  "GitHub API URL attributes.")

;;;###autoload
(defun lazygithub-retriever (endpoint)
  "Retrieve resources from GitHub ENDPOINT."
  (interactive "sEnter a GitHub API endpoint: ")
  (lazygit-view-retrieved-json
   (concat lazygithub-baseurl endpoint lazygithub-attr)
   "*lazygithub*"
   `(("Authorization" . ,(concat "token " (lazygit-secret-from-authinfo "github.com"))))))

;;;###autoload
(defun lazygithub-get-values (endpoint keys)
  "Retrieve values from KEYS of GitHub ENDPOINT JSON resources."
  (lazygit-get-values
   (concat lazygithub-baseurl endpoint lazygithub-attr)
   keys
   `(("Authorization" . ,(concat "token " (lazygit-secret-from-authinfo "github.com"))))))

;;;###autoload
(defun lazygithub-clone-or-pull-repo ()
  "Clone or pull repository to `lazygithub-directory'."
  (interactive)
  (lazygit-clone-or-pull-repo (lazygithub-get-values "user/repos" (list 'full_name
                                                                        'name
                                                                        'ssh_url))
                              'full_name 'name 'ssh_url lazygithub-directory))

;;;###autoload
(defun lazygithub-clone-or-pull-all ()
  "Clone or pull ALL GitHub repositories to `lazygitlab-directory'."
  (interactive)
  (let ((repos (lazygithub-get-values "user/repos" (list 'full_name 'ssh_url))))
    (lazygit-clone-or-pull-batch repos lazygithub-directory 'full_name 'ssh_url)
    (message "Cloned or pulled all GitHub projects to %s" lazygithub-directory)))

;;;###autoload
(defun lazygithub-pull-all (arg)
  "Git pull all projects in `lazygithub-directory'.
If `prefix' only look for git repos ARG deep.  Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-pull-all arg lazygithub-directory))

;;;###autoload
(defun lazygithub-status-all (arg)
  "View status of all projects in `lazygithub-directory'.
If `prefix' only look for git repos ARG deep.  Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-status-all arg lazygithub-directory))

;;;###autoload
(defvar lazygithub-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ca") 'lazygithub-clone-or-pull-all)
    (define-key map (kbd "cr") 'lazygithub-clone-or-pull-repo)
    (define-key map (kbd "p") 'lazygithub-pull-all)
    (define-key map (kbd "r") 'lazygithub-retriever)
    (define-key map (kbd "s") 'lazygithub-status-all)
    map)
  "Keymap for `lazygithub' commands.")
(fset 'lazygithub-map lazygithub-map)

(provide 'lazygithub)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; lazygithub.el ends here
