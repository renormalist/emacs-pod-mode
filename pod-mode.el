;;; pod-mode.el --- Major mode for editing .pod-files

;;; POD is the Plain Old Documentation format of Perl.

;;; Copyright 2003-2010 Steffen Schwigon

;;; Author: Steffen Schwigon <ss5@renormalist.net>
;;;
;;; Keywords: emacs mode perl pod
;;; X-URL: http://search.cpan.org/~schwigon/pod-mode/

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Tested on i386-linux with XEmacs 21.4.
;;; Tested on i386-linux with GNU Emacs 21.2.1.
;;; Tested on i386-windows-2k with XEmacs 21.4.

;;; Commentary:

;;; This mode is built with help of the
;;; "Emacs language mode creation tutorial" at
;;;
;;;   http://two-wugs.net/emacs/mode-tutorial.html
;;;
;;; which disapeared from the net and is now hosted at
;;;
;;;   http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;;;

;;; Usage:

;;; Put this file into your load-path and the following into your ~/.emacs:
;;;
;;;    (require 'pod-mode)
;;;
;;;
;;; To associate pod-mode with .pod files add the following to your ~/.emacs
;;;
;;;    (setq auto-mode-alist
;;;       (append auto-mode-alist
;;;         '(("\\.pod$" . pod-mode))))
;;;
;;;
;;; To automatically turn on font-lock-mode add the following to your ~/.emacs
;;;
;;;    (add-hook 'pod-mode-hook 'font-lock-mode)
;;;

;;; Code:

(require 'cl)

(defgroup pod-mode nil
  "Mode for editing POD files"
  :group 'faces)

(defgroup pod-mode-faces nil
  "Faces for highlighting POD constructs"
  :prefix "pod-mode-"
  :group 'pod-mode)

(defface pod-mode-command-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight POD commands"
  :group 'pod-mode-faces)

(defface pod-mode-head-face
  '((t (:inherit pod-mode-command-face)))
  "Face used to highlight =head commands"
  :group 'pod-mode-faces)

(defface pod-mode-command-text-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark))
     )
    (t (:weight bold :slant italic)))
  "Face used to highlight text after POD commands"
  :group 'pod-mode-faces)

(defface pod-mode-verbatim-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Face used to highlight verbatim paragraphs in POD"
  :group 'pod-mode-faces)

(defface pod-mode-formatting-code-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used to highlight formatting codes in POD"
  :group 'pod-mode-faces)

(defface pod-mode-alternative-formatting-code-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Alternative face used to highlight formatting codes in POD.
This is used for E<> escapes and for the link target in L<>
escapes."
  :group 'pod-mode-faces)

(defface pod-mode-string-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight quoted strings in POD"
  :group 'pod-mode-faces)

;; default variables
(defvar pod-mode-hook nil)

;;; Version: 1.01
(defvar pod-version "1.01"
  "Version of POD mode.")

;; keymap
(defvar pod-mode-map nil "Keymap for POD major mode.")
(if pod-mode-map nil
  (let ((map (make-sparse-keymap)))
    ;; insert (define-key map ...) stuff here
    (setq pod-mode-map map)))

;; syntax highlighting: standard keywords
(let ((head-sizes '(1.9 1.7 1.5 1.3)) ;; FIXME: completely made up
      (heads))
  (let ((i 0))
    (setq heads (mapcar (lambda (s)
                          (setq i (+ i 1))
                          (cons i s))
                        head-sizes)))
  (defconst pod-font-lock-keywords-1
    (append
     (loop for (n . s) in heads collect
           (let ((head-face-name (intern (format "pod-mode-head%d-face" n)))
                 (text-face-name (intern (format "pod-mode-head%d-text-face" n))))
             (eval `(defface ,head-face-name
                      '((t (:inherit pod-mode-head-face :height ,s)))
                      ,(format "Face used to highlight head%d commands" n)
                      :group 'pod-mode-faces))
             (eval `(defface ,text-face-name
                      '((t (:inherit pod-mode-command-text-face :height ,s)))
                      ,(format "Face used to hightlight text in head%d commands" n)
                      :group 'pod-mode-faces))
             `(,(format "^\\(=head%d\\)\\(.*\\)" n)
               (1 (quote ,head-face-name))
               (2 (quote ,text-face-name)))))
     `((,(concat "^\\(="
                 (regexp-opt '("item" "over" "back" "cut" "pod"
                               "for" "begin" "end" "encoding") t)
                 "\\)\\(.*\\)")
        (1 'pod-mode-command-face)
        (3 'pod-mode-command-text-face))
       ("^[ \t]+\\(.*\\)$" 1 'pod-mode-verbatim-face)))
    "Minimal highlighting expressions for POD mode."))

;; syntax highlighting: additional keywords
(defconst pod-font-lock-keywords-2
  (append pod-font-lock-keywords-1 '())
  "Additional Keywords to highlight in POD mode.")

;; syntax highlighting: even more keywords
(defconst pod-font-lock-keywords-3
  (append pod-font-lock-keywords-2
          '(
            ("[IBCFXZS]<\\([^>]*\\)>" 1 'pod-mode-formatting-code-face)
            ("L<\\(\\([^|>]*\\)|\\)\\([^>]+\\)>"
             (2 'pod-mode-formatting-code-face)
             (3 'pod-mode-alternative-formatting-code-face))
            ("L<\\([^|>]+\\)>" 1 'pod-mode-alternative-formatting-code-face)
            ("E<\\([^>]*\\)>" 1 'pod-mode-alternative-formatting-code-face)
            ("\"\\([^\"]+\\)\"" 0 'pod-mode-string-face)
            ))
  "Balls-out highlighting in POD mode.")

;; default level of highlight to maximum
(defvar pod-font-lock-keywords pod-font-lock-keywords-3
  "Default highlighting expressions for POD mode.")

;; no special indenting, just pure text mode
(defun pod-indent-line ()
  "Indent current line as POD code.
Does nothing yet."
  (interactive)
  )

;; no special syntax table
(defvar pod-mode-syntax-table nil
  "Syntax table for `pod-mode'.")

;; create and activate syntax table
(defun pod-create-syntax-table ()
  (if pod-mode-syntax-table
      ()
    (setq pod-mode-syntax-table (make-syntax-table))
    (set-syntax-table pod-mode-syntax-table)
    ))

(defun pod-add-support-for-outline-minor-mode ()
  "Provides additional menus from =head lines in `outline-minor-mode'."
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "=head[1234] ")
  (make-local-variable 'outline-level)
  (setq outline-level
        (function (lambda ()
                    (save-excursion
                      (string-to-int (buffer-substring (+ (point) 5) (+ (point) 6)))
                      ))))
  )

(defun pod-enable-weaver-collector-keywords (collectors)
  (let* ((keyword-list (mapcar (lambda (collector)
                                 (symbol-name (getf collector 'command)))
                               collectors))
         (keyword-re (concat "^=" (regexp-opt keyword-list t))))
    (setf pod-font-lock-keywords
          (append pod-font-lock-keywords
                  `((,keyword-re 0 'pod-mode-command-face))
                  `((,(concat keyword-re "\\(.*\\)") 2 'pod-mode-command-text-face))))
    (setq font-lock-mode-major-mode nil)
    (font-lock-fontify-buffer)))

(defun pod-enable-weaver-features (weaver-config)
  (pod-enable-weaver-collector-keywords (getf weaver-config 'collectors))
  (message "Pod::Weaver keywords loaded."))

(defvar pod-weaver-config-buffer "")

(defun pod-load-weaver-config (dir)
  "Load additional pod keywords from dist.ini/weaver.ini in DIR."
  (let* ((proc (start-process-shell-command
                (concat "weaverconf-" (buffer-name (current-buffer)))
                nil (format "cd %s; dzil weaverconf -f lisp" dir))))
    (make-local-variable 'pod-weaver-config-buffer)
    (set-process-filter
     proc (lambda (proc str)
            (setq pod-weaver-config-buffer (concat pod-weaver-config-buffer str))))
    (set-process-sentinel
     proc (lambda (proc event)
            (if (string-equal event "finished\n")
                (let ((weaver-config
                       (ignore-errors
                         (eval (car (read-from-string pod-weaver-config-buffer))))))
                  (if weaver-config (pod-enable-weaver-features weaver-config))))
            (setq pod-weaver-config-buffer "")))))

(defun pod-add-support-for-weaver ()
  (let ((project-root (ignore-errors (eproject-maybe-turn-on))))
    (if project-root (pod-load-weaver-config project-root))))

;; main
(defun pod-mode ()
  "Major mode for editing POD files (Plain Old Documentation for Perl)."
  (interactive)
  (kill-all-local-variables)
  (pod-create-syntax-table)
  (use-local-map pod-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pod-font-lock-keywords 't))
  ;; (make-local-variable 'indent-line-function)
  ;; (setq indent-line-function 'pod-indent-line)
  (setq major-mode 'pod-mode)
  (setq mode-name "POD")
  (setq imenu-generic-expression '((nil "^=head[1234] +\\(.*\\)" 1)))
  (run-hooks 'pod-mode-hook)
  (pod-add-support-for-outline-minor-mode)
  (pod-add-support-for-weaver)
  )

(provide 'pod-mode)

;;; pod-mode.el ends here
