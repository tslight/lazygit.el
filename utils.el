;;; utils.el --- Various utility functions to support lazygit

;;; Commentary:

;; So far only GitHub & GitLab are supported.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defun read-file (file)
  "Return FILE content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun filter-keys (list keys)
  "Filter a LIST of association lists by a list of KEYS."
  (mapcar (lambda (al)
            (mapcar (lambda (key)
                      (assoc key al))
                    keys))
          list))

(defun get-assoc-list (list key value)
  "Return an alist from LIST of alists that has KEY VALUE pair."
  (car (cl-remove-if nil (mapcar
                          (lambda (e) (if (equal value (cdr (assoc key e))) e))
                          list))))

(defun flatten-json (items)
  "Turn a list of json arrays (ITEMS) into a single json array."
  (concat "[" (mapconcat
               (lambda (item)
                 (let*
                     ((trimmed (replace-regexp-in-string "^\\[\\]$" "" item))
                      (trimmed (replace-regexp-in-string "^\\[" "" trimmed))
                      (trimmed (if (equal item (car (last items)))
                                   (replace-regexp-in-string "\\]$" "" trimmed)
                                 (replace-regexp-in-string "\\]$" "," trimmed))))
                   trimmed))
               items "")
          "]"))

(defun display-async-shell-command-buffer (command buffer-name &optional erase)
  "Display the results of asynchronous COMMAND in BUFFER-NAME.
If ERASE is true erase the buffer first"
  (let ((buffer (get-buffer-create buffer-name)))
    (if erase (progn (set-buffer buffer) (erase-buffer)))
    (display-buffer buffer)
    (start-process-shell-command command buffer command)))

(provide 'utils)
;;; utils.el ends here
