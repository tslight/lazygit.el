;;; lazygitlab.el --- GitLab API Client -*- lexical-binding: t; -*-

;;; Commentary:

;; GitLab API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; lazygitlab-clone-or-pull-repo.

;; If `magit' is installed, this will be used, otherwise we shell out to git,
;; so obviously, that needs to be installed on your system.

;; You can clone or pull all repositories in one fail swoop with
;; lazygitlab-clone-or-pull-all.  This will not use `magit', unless called with
;; prefix argument.

;; You can also clone or pull all the repositories under a given group with
;; lazygitlab-clone-or-pull-group.  This will not use `magit', unless called with
;; prefix argument.

;; lazygitlab-retriever allows arbitrary querying of endpoints.  If
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
;; URL: https://github.com/purcell/package-lint
;; Package-Requires: ((emacs "24.4"))
;; Version: 0

;;; Code:
(require 'lazygit)

(defcustom lazygitlab-token-file (concat user-emacs-directory ".gitlab.token")
  "File to store `GitLab' API Personal Access Token in."
  :group 'lazygit-tokens
  :type 'file)

(defun lazygitlab-install-token (token)
  "Prompt for `GitLab' TOKEN and write it to `lazygitlab-token-file'."
  (interactive "sEnter your GitLab Personal Access Token: ")
  (write-region token nil lazygitlab-token-file)
  token)

(defun lazygitlab-token-p ()
  "Check it `lazygitlab-token-file' exists and is non-empty."
  (if (and (file-readable-p lazygitlab-token-file)
           (file-regular-p lazygitlab-token-file))
      (lazygit-read-file lazygitlab-token-file)
    (call-interactively #'lazygitlab-install-token)))

(defvar lazygitlab-baseurl "https://gitlab.com/api/v4/")
(defvar lazygitlab-attr "?pagination=keyset&per_page=100&order_by=id&sort=asc&membership=true")
(defvar lazygitlab-token (lazygitlab-token-p))
(defvar lazygitlab-auth-header `(("PRIVATE-TOKEN" . ,lazygitlab-token)))

(defun lazygitlab-retriever (endpoint)
  "Retrieve resources from `GitLab' ENDPOINT."
  (interactive "sEnter GitLab API endpoint: ")
  (lazygit-view-retrieved-json (concat lazygitlab-baseurl endpoint lazygitlab-attr)
                               "*lazygitlab*"
                               lazygitlab-auth-header))

(defun lazygitlab-get-values (endpoint keys)
  "Retrieve values from KEYS of `GitLab' ENDPOINT JSON resources."
  (lazygit-get-values (concat lazygitlab-baseurl endpoint lazygitlab-attr)
                      keys
                      lazygitlab-auth-header))

(defun lazygitlab-clone-or-pull-project (directory)
  "Clone or pull repository to DIRECTORY."
  (interactive "DDirectory to clone GitLab project to: ")
  (lazygit-clone-or-pull-repo
   (lazygitlab-get-values "projects" (list 'path_with_namespace
                                           'name
                                           'ssh_url_to_repo))
   'path_with_namespace
   'name
   'ssh_url_to_repo
   directory))

(defun lazygitlab-subgroups (groups prefix)
  "Return a list of subgroups from GROUPS using PREFIX."
  (remove nil
          (mapcar
           (lambda (g)
             (if (string-prefix-p prefix
                                  (cdr (assoc 'full_path g)))
                 g))
           groups)))

(defun lazygitlab-group-projects (groups)
  "Return a list of projects from GROUPS."
  (let ((ids (mapcar (lambda (g) (cdr (assoc 'id g))) groups)))
    (apply #'append
           (remove nil
                   (mapcar
                    (lambda (id)
                      (lazygitlab-get-values
                       (concat "groups/" (number-to-string id) "/projects")
                       (list 'path_with_namespace
                             'name
                             'ssh_url_to_repo)))
                    ids)))))

(defun lazygitlab-clone-or-pull-group (directory)
  "Prompt for a group, then clone that repository to DIRECTORY."
  (interactive "DDirectory to clone GitLab group to: ")
  (let* ((groups (lazygitlab-get-values "groups" (list 'full_path 'id)))
         (paths (mapcar (lambda (g) (cdr (assoc 'full_path g))) groups))
         (choice (completing-read "Group or subgroup to batch clone: " paths))
         (groups (lazygitlab-subgroups groups choice))
         (projects (lazygitlab-group-projects groups)))
    (lazygit-clone-or-pull-batch projects
                                 directory
                                 'path_with_namespace
                                 'ssh_url_to_repo)))

(defun lazygitlab-clone-or-pull-all (directory)
  "Clone or pull ALL projects to DIRECTORY."
  (interactive "DDirectory to clone ALL GitLab projects to: ")
  (let ((projects (lazygitlab-get-values "projects"
                                         (list 'path_with_namespace 'ssh_url_to_repo))))
    (lazygit-clone-or-pull-batch projects
                                 directory
                                 'path_with_namespace
                                 'ssh_url_to_repo)))

(provide 'lazygitlab)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; lazygitlab.el ends here
