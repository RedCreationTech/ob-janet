;;; ob-janet.el --- Org-Babel support for the Janet programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;; Author: Your Name <your.email@example.com>
;; Keywords: org, babel, literate programming, janet
;; URL: https://github.com/YOUR_USERNAME/ob-janet
;; Version: 1.0.4
;; Package-Requires: ((emacs "25.1") (org "9.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; This library provides Org-Babel support for evaluating Janet
;; (https://janet-lang.org/) source code blocks.
;;
;; It supports:
;; - Simple code block execution.
;; - Passing variables to Janet code blocks via the :vars header argument.
;; - Persistent sessions via the :session header argument.
;;
;; To use, add the following to your Emacs configuration:
;;
;; (use-package ob-janet
;;   :ensure t ;; or :straight t
;;   :config
;;   (with-eval-after-load 'org
;;     (add-to-list 'org-babel-load-languages '(janet . t))
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      org-babel-load-languages)))

;;; Code:

(require 'ob)
(require 'ob-core) ; Explicitly require ob-core for compiler warnings
(require 'ob-comint)

(defvar org-babel-janet-buffers '()
  "An association list of Janet session names and their corresponding comint buffers.
Each element is of the form (SESSION-NAME . BUFFER).")

(defun org-babel-janet-var-to-janet (var)
  "Convert an Emacs Lisp variable VAR into a Janet source code string."
  (cond
   ((stringp var)
    ;; Escape double quotes within the string.
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" var)))
   ((numberp var) (format "%s" var))
   ((eq var t) "true")
   ((eq var nil) "false")
   ((symbolp var)
    (if (keywordp var)
        (format "%s" var) ; Janet keywords are like Elisp keywords
      (format "'%s" var)))
   ((vectorp var)
    (format "@[%s]"
            (mapconcat #'org-babel-janet-var-to-janet (seq-into var 'list) " ")))
   ((listp var)
    (if (org-babel-list-p var)
        ;; A regular list, convert to a Janet tuple.
        (format "@[%s]"
                (mapconcat #'org-babel-janet-var-to-janet var " "))
      ;; An association list, convert to a Janet struct (table).
      (format "@{%s}"
              (mapconcat
               (lambda (pair)
                 (format "%s %s"
                         (org-babel-janet-var-to-janet (car pair))
                         (org-babel-janet-var-to-janet (cdr pair))))
               var " "))))
   (t (error "Cannot convert Elisp type '%s' to Janet" (type-of var)))))

(defun org-babel-variable-assignments:janet (params)
  "Return a list of Janet `def` statements for variables defined in PARAMS."
  (mapcar
   (lambda (pair)
     (let ((var (car pair))
           (val (cdr pair)))
       (format "(def %s %s)" var (org-babel-janet-var-to-janet val))))
   (org-babel--get-vars params)))

(defun org-babel-janet-initiate-session (&optional session)
  "Initiate a Janet session if needed.
Returns the comint buffer associated with the SESSION name."
  (when session
    (let* ((session-name (if (stringp session) session "default"))
           (buffer-name (format "*janet-repl-%s*" session-name))
           ;; Use standard `assoc` to find the pair (SESSION-NAME . BUFFER)
           (pair (assoc session-name org-babel-janet-buffers)))
      (if (and pair (buffer-live-p (cdr pair)))
          (cdr pair) ; Return the existing live buffer
        (let ((new-buffer (make-comint buffer-name "janet" nil "-r")))
          (with-current-buffer new-buffer
            ;; Janet's REPL prompt is ">> ".
            (setq-local comint-prompt-regexp "^>> ")
            (comint-mode))
          ;; Update or add the session to the alist.
          (if pair
              (setcdr pair new-buffer) ; Update existing pair with new buffer
            (push (cons session-name new-buffer) org-babel-janet-buffers)) ; Add new pair
          new-buffer)))))

(defun org-babel-execute:janet (body params)
  "Execute a block of Janet code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((results (cdr (assq :results params)))
         (session (cdr (assq :session params)))
         (vars (org-babel-variable-assignments:janet params))
         (full-body (mapconcat #'identity (append vars (list body)) "\n")))
    (if session
        ;; Session-based execution
        (let* ((buffer (org-babel-janet-initiate-session session))
               ;; Use the robust `org-babel-comint-with-output' macro.
               ;; The first argument to this macro must be a list containing
               ;; the buffer and an optional end-of-output indicator.
               ;; We pass `(list buffer nil)` so the macro uses the
               ;; buffer-local `comint-prompt-regexp` we've set.
               (comint-results
                (car
                 (org-babel-comint-with-output (list buffer nil)
                   (insert full-body)
                   (comint-send-input nil t)))))
          (org-babel-result-cond (cdr (assq :result-params params))
            comint-results
            (org-babel-read-result-from-string comint-results)))
      ;; Command-line execution
      (let* ((cmdline (or (cdr (assq :cmdline params)) ""))
             (full-cmd (format "janet %s -e" cmdline)))
        (org-babel-eval full-cmd full-body)))))

(provide 'ob-janet)

;;; ob-janet.el ends here
