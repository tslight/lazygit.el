;;; lazygithub.el --- GitHub API Client -*- lexical-binding: t; -*-

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

;; By default these tokens will be stored in $HOME/.emacs.d/.github.token and
;; $HOME/.emacs.d/.github.token, however these locations can be customised with
;; `customize' or `setq'.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; URL: https://github.com/purcell/package-lint
;; Package-Requires: ((emacs "24.4"))

;;; Code:
(require 'lazygit)

(defcustom lazygithub-directory (expand-file-name "~/src/github")
  "File to store API Personal Access Tokens in."
  :group 'lazygit
  :type 'directory)

(defun lazygithub-install-token (token)
  "Prompt for `GitHub' TOKEN and write it to `lazygithub-token-file'."
  (interactive "sEnter your GitHub Personal Access Token: ")
  (add-to-list 'lazygit-token-alist `("GitHub" . ,token))
  (write-region (format "%S" lazygit-token-alist) nil lazygit-token-file)
  (concat "token " (cdr (assoc "GitHub" lazygit-token-alist))))

;;;###autoload
(defun lazygithub-token-p ()
  "Check it `lazygit-token-file' exists and is non-empty."
  (if (and (file-readable-p lazygit-token-file)
           (file-regular-p lazygit-token-file)
           (lazygit-read-alist-from-file lazygit-token-file)
           (assoc "GitHub" (lazygit-read-alist-from-file lazygit-token-file)))
      (concat "token "
              (cdr (assoc "GitHub" (lazygit-read-alist-from-file lazygit-token-file))))
    (call-interactively #'lazygithub-install-token)))

(defvar lazygithub-baseurl "https://api.github.com/")
(defvar lazygithub-attr "?per_page=100&page=1")

;;;###autoload
(defun lazygithub-retriever (endpoint)
  "Retrieve resources from GitHub ENDPOINT."
  (interactive "sEnter a GitHub API endpoint: ")
  (lazygit-view-retrieved-json (concat lazygithub-baseurl endpoint lazygithub-attr)
                               "*lazygithub*"
                               `(("Authorization" . ,(lazygithub-token-p)))))

;;;###autoload
(defun lazygithub-get-values (endpoint keys)
  "Retrieve values from KEYS of GitHub ENDPOINT JSON resources."
  (lazygit-get-values (concat lazygithub-baseurl endpoint lazygithub-attr)
                      keys
                      `(("Authorization" . ,(lazygithub-token-p)))))

;;;###autoload
(defun lazygithub-clone-or-pull-repo ()
  "Clone or pull repository to `lazygitlab-directory'."
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
    (lazygit-clone-or-pull-batch repos lazygithub-directory 'full_name 'ssh_url)))

(provide 'lazygithub)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; lazygithub.el ends here
