;;; gitlab.el --- GitLab API client for Emacs

;;; Commentary:

;; GitLab API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; gitlab/clone-or-pull-repo.

;; If `magit' is installed, this will be used, otherwise we shell out to git,
;; so obviously, that needs to be installed on your system.

;; You can also clone or pull all repositories in one fail swoop with
;; gitlab/clone-or-pull-all.  This will not use `magit', unless called with
;; prefix argument.

;; gitlab/retriever allows arbitrary querying of endpoints.  If
;; `json-navigator' is installed, the JSON results will be opened in a fancy
;; tree hiearchy browser.  If not, you can view the JSON results pretty printed
;; in a dedicated buffer.

;; You will be asked to enter your Personal Access Token the first time you
;; run a command.

;; Generate a this here: https://gitlab.com/profile/personal_access_tokens

;; By default these tokens will be stored in $HOME/.emacs.d/.gitlab.token and
;; $HOME/.emacs.d/.github.token, however these locations can be customised with
;; `customize' or `setq'.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'lazygit)

(defcustom gitlab/token-file (concat user-emacs-directory ".gitlab.token")
  "File to store GitLab API Personal Access Token in."
  :group 'lazygit/tokens
  :type 'file)

(defun gitlab/install-token (token)
  "Prompt for GitLab TOKEN and write it to `gitlab/token-file'."
  (interactive "sEnter your GitLab Personal Access Token: ")
  (write-region token nil gitlab/token-file)
  token)

(defun gitlab/token? ()
  "Check it `gitlab/token-file' exists and is non-empty."
  (if (and (file-readable-p gitlab/token-file)
	   (file-regular-p gitlab/token-file))
      (read-file gitlab/token-file)
    (call-interactively #'gitlab/install-token)))

(defun gitlab/retriever (endpoint)
  "Retrieve resources from GitLab ENDPOINT."
  (interactive "sEnter GitLab API endpoint: ")
  (url/view-retrieved-json (concat "https://gitlab.com/api/v4/"
                                   endpoint
                                   "?pagination=keyset&per_page=100&"
                                   "order_by=id&sort=asc"
                                   "&membership=true")
			   "*lazygitlab*"
                           ;; https://stackoverflow.com/a/24188208
                           `(("PRIVATE-TOKEN" . ,(gitlab/token?)))))

(defun gitlab/get-values (endpoint keys)
  "Retrieve values from KEYS of GitLab ENDPOINT json resources."
  (url/get-values (concat "https://gitlab.com/api/v4/"
                          endpoint
                          "?pagination=keyset&per_page=100&"
                          "order_by=id&sort=asc"
                          "&membership=true")
                  keys
                  ;; https://stackoverflow.com/a/24188208
                  `(("PRIVATE-TOKEN" . ,(gitlab/token?)))))

(defun gitlab/clone-or-pull-project (directory)
  "Clone or pull repo to DIRECTORY."
  (interactive "DDirectory to clone to: ")
  (git/clone-or-pull-repo (gitlab/get-values "projects" (list 'path_with_namespace
                                                              'name
                                                              'ssh_url_to_repo))
                          'path_with_namespace
                          'name
                          'ssh_url_to_repo
                          directory))

(defun gitlab/subgroups (groups prefix)
  "Return a list of subgroups from GROUPS using PREFIX."
  (cl-remove-if nil
                (mapcar
                 (lambda (g)
                   (if (string-prefix-p prefix
                                        (cdr (assoc 'full_path g)))
                       g))
                 groups)))

(defun gitlab/group-projects (groups)
  "Return a list of projects from GROUPS."
  (let ((ids (mapcar (lambda (g) (cdr (assoc 'id g))) groups)))
    (apply #'append
           (cl-remove-if nil
                         (mapcar
                          (lambda (id)
                            (gitlab/get-values
                             (concat "groups/" (number-to-string id) "/projects")
                             (list 'path_with_namespace
                                   'name
                                   'ssh_url_to_repo)))
                          ids)))))

(defun gitlab/clone-or-pull-group (directory)
  "Prompt for a group, then clone that repo to DIRECTORY."
  (interactive "DDirectory to clone to: ")
  (let* ((groups (gitlab/get-values "groups" (list 'full_path 'id)))
         (paths (mapcar (lambda (g) (cdr (assoc 'full_path g))) groups))
         (choice (completing-read "Group or subgroup to batch clone: " paths))
         (groups (gitlab/subgroups groups choice))
         (paths (mapcar (lambda (g) (cdr (assoc 'full_path g))) groups))
         (projects (gitlab/group-projects groups)))
    (mapc (lambda (p) (make-directory (concat directory "/" p) t)) paths)
    (mapc (lambda (p)
            (git/clone-or-pull
             (concat directory "/" (cdr (assoc 'path_with_namespace p)))
             (cdr (assoc 'ssh_url_to_repo p))))
          projects)))

(defalias 'gl/api 'gitlab/retriever)
(defalias 'gl/grp 'gitlab/clone-or-pull-group)
(defalias 'gl/prj 'gitlab/clone-or-pull-project)

(provide 'gitlab)
;;; gitlab.el ends here
