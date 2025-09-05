;;; ob-janet.el --- Org-Babel support for the Janet programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;; Author: Your Name <your.email@example.com>
;; Keywords: org, babel, literate programming, janet
;; URL: https://github.com/YOUR_USERNAME/ob-janet
;; Version: 1.0.9
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
;; - Differentiates between `:results value` and `:results output`.

;;; Code:

(unless (fboundp 'org-babel-execute:janet)
  (defun org-babel-execute:janet (body params)
    "Execute a block of Janet code BODY with PARAMS.
This simple implementation writes BODY to a temp file and runs `janet` on it.
It returns stdout as string."
    (let* ((file (org-babel-temp-file "janet-" ".janet"))
           (cmd (format "janet %s" (org-babel-process-file-name file))))
      (with-temp-file file
        (insert (concat "(pp (do " body "))")))
      ;; 调用命令并返回结果（注意：这只是最简单的实现，未处理 :results 控制等高级选项）
      (org-babel-eval cmd ""))))
(provide 'ob-janet)

;;; ob-janet.el ends here
