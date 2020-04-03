;;; gitlab.el --- GitLab API client for Emacs

;;; Commentary:

;; GitLab API client for Emacs.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'lazygit (expand-file-name "lazygit.el"))

(defun gitlab/retriever (endpoint)
  "Retrieve resources from GitLab ENDPOINT."
  (interactive "sEnter an endpoint: ")
  (url/view-retrieved-json (concat "https://gitlab.com/api/v4/"
                                   endpoint
                                   "?pagination=keyset&per_page=100&"
                                   "order_by=id&sort=asc"
                                   "&membership=true")
                           ;; https://stackoverflow.com/a/24188208
                           `(("PRIVATE-TOKEN" . ,gitlab/api-key))))

(defun gitlab/get-values (endpoint keys)
  "Retrieve values from KEYS of GitLab ENDPOINT json resources."
  (url/get-values (concat "https://gitlab.com/api/v4/"
                          endpoint
                          "?pagination=keyset&per_page=100&"
                          "order_by=id&sort=asc"
                          "&membership=true")
                  keys
                  ;; https://stackoverflow.com/a/24188208
                  `(("PRIVATE-TOKEN" . ,gitlab/api-key))))

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

(defalias 'gl/cpgrp 'gitlab/clone-or-pull-group)
(defalias 'gl/cpprj 'gitlab/clone-or-pull-project)

(provide 'gitlab)
;;; gitlab.el ends here
