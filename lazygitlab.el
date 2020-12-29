;;; lazygitlab.el --- GitLab API Client -*- lexical-binding: t; -*-

;;; Commentary:

;; GitLab API client for Emacs.

;; So far I have implemented functions to clone or pull repositories from a
;; list of repositories available to the authenticated user, with
;; lazygitlab-clone-or-pull-repo.

;; You can clone or pull all repositories in one fail swoop with
;; lazygitlab-clone-or-pull-all.

;; You can also clone or pull all the repositories under a given group with
;; lazygitlab-clone-or-pull-group.

;; lazygitlab-retriever allows arbitrary querying of endpoints.  You can view
;; the JSON results pretty printed in a dedicated buffer.

;; You will be asked to enter your Personal Access Token the first time you
;; run a command.

;; Generate a this here: https://gitlab.com/profile/personal_access_tokens

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
(defcustom lazygitlab-directory (expand-file-name "~/src/gitlab")
  "Directory to clone projects to."
  :group 'lazygit
  :type 'directory)

;;;###autoload
(defvar lazygitlab-baseurl "https://gitlab.com/api/v4/"
  "GitLab API base URL.")

;;;###autoload
(defvar lazygitlab-attr
  "?pagination=keyset&per_page=100&order_by=id&sort=asc&membership=true"
  "GitLab API URL attributes.")

;;;###autoload
(defun lazygitlab-retriever (endpoint)
  "Retrieve resources from `GitLab' ENDPOINT."
  (interactive "sEnter GitLab API endpoint: ")
  (lazygit-view-retrieved-json
   (concat lazygitlab-baseurl endpoint lazygitlab-attr)
   "*lazygitlab*"
   `(("PRIVATE-TOKEN" . ,(lazygit-secret-from-authinfo "gitlab.com")))))

;;;###autoload
(defun lazygitlab-get-values (endpoint keys)
  "Retrieve values from KEYS of `GitLab' ENDPOINT JSON resources."
  (lazygit-get-values
   (concat lazygitlab-baseurl endpoint lazygitlab-attr)
   keys
   `(("PRIVATE-TOKEN" . ,(lazygit-secret-from-authinfo "gitlab.com")))))

;;;###autoload
(defun lazygitlab-clone-or-pull-project ()
  "Clone or pull repository to `lazygitlab-directory'."
  (interactive)
  (lazygit-clone-or-pull-repo
   (lazygitlab-get-values "projects" (list 'path_with_namespace
                                           'name
                                           'ssh_url_to_repo))
   'path_with_namespace
   'name
   'ssh_url_to_repo
   lazygitlab-directory))

;;;###autoload
(defun lazygitlab-subgroups (groups prefix)
  "Return a list of subgroups from GROUPS using PREFIX."
  (remove nil
          (mapcar
           (lambda (g)
             (if (string-prefix-p prefix
                                  (cdr (assoc 'full_path g)))
                 g))
           groups)))

;;;###autoload
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

;;;###autoload
(defun lazygitlab-clone-or-pull-group ()
  "Prompt for a group, then clone that repository to `lazygitlab-directory'."
  (interactive)
  (let* ((groups (lazygitlab-get-values "groups" (list 'full_path 'id)))
         (paths (mapcar (lambda (g) (cdr (assoc 'full_path g))) groups))
         (choice (completing-read "Group or subgroup to batch clone: " paths))
         (groups (lazygitlab-subgroups groups choice))
         (projects (lazygitlab-group-projects groups)))
    (lazygit-clone-or-pull-batch projects
                                 lazygitlab-directory
                                 'path_with_namespace
                                 'ssh_url_to_repo)
    (message "Cloned or pulled group to %s/%s" lazygitab-directory choice)))

;;;###autoload
(defun lazygitlab-clone-or-pull-all ()
  "Clone or pull ALL projects to `lazygitlab-directory'."
  (interactive)
  (let ((projects (lazygitlab-get-values "projects"
                                         (list 'path_with_namespace 'ssh_url_to_repo))))
    (lazygit-clone-or-pull-batch projects
                                 lazygitlab-directory
                                 'path_with_namespace
                                 'ssh_url_to_repo)
    (message "Cloned or pulled all GitLab projects to %s" lazygitlab-directory)))

;;;###autoload
(defun lazygitlab-pull-all (arg)
  "Git pull all projects in `lazygitlab-directory'.
If `prefix' only look for git repos ARG deep.  Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-pull-all arg lazygitlab-directory))

;;;###autoload
(defun lazygitlab-status-all (arg)
  "View status of all projects in `lazygitlab-directory'.
If `prefix' only look for git repos ARG deep.  Defaults to `lazygit-maxdepth'."
  (interactive "p")
  (lazygit-status-all arg lazygitlab-directory))

;;;###autoload
(defvar lazygitlab-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ca") 'lazygitlab-clone-or-pull-all)
    (define-key map (kbd "cg") 'lazygitlab-clone-or-pull-group)
    (define-key map (kbd "cp") 'lazygitlab-clone-or-pull-project)
    (define-key map (kbd "p") 'lazygitlab-pull-all)
    (define-key map (kbd "r") 'lazygitlab-retriever)
    (define-key map (kbd "s") 'lazygitlab-status-all)
    map)
  "Keymap for `lazygitlab' commands.")
(fset 'lazygitlab-map lazygitlab-map)

(provide 'lazygitlab)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; lazygitlab.el ends here
