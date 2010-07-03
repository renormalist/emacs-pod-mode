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

(defface pod-mode-formatting-code-i-face
  '((t (:inherit pod-mode-formatting-code-face :slant italic)))
  "Face used to highlight I<> formatting codes in POD"
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

;; syntax highlighting: standard keywords
(let* ((head-sizes '(1.9 1.7 1.5 1.3))
       (heads (loop for i from 1 to (length head-sizes) collect
                    (cons i (nth (- i 1) head-sizes)))))
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
                               "for" "begin" "end" "encoding"))
                 "\\)\\(.*\\)")
        (1 'pod-mode-command-face)
        (2 'pod-mode-command-text-face))
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
            ("[BCFXZS]<\\([^>]*\\)>" 1 'pod-mode-formatting-code-face)
            ("I<\\([^>]*\\)>" 1 'pod-mode-formatting-code-i-face)
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

(defvar pod-weaver-section-keywords nil)
(make-local-variable 'pod-weaver-section-keywords)

(defun pod-linkable-sections-for-buffer (buffer &optional section-keywords)
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (loop while (re-search-forward
                     (concat "^="
                             (regexp-opt
                              (append
                               (loop for i from 1 to 4
                                     collect (concat "head" (int-to-string i)))
                               section-keywords))
                             "\s+\\(.*\\)$")
                     nil t)
              collect (match-string-no-properties 1))))))

(defun pod-linkable-sections-for-module (module)
  (with-current-buffer (get-buffer-create (concat "*POD " module "*"))
    (unwind-protect
        (progn
          (kill-all-local-variables)
          (erase-buffer)
          (text-mode)
          (let ((default-directory "/"))
            (call-process "perldoc" nil (current-buffer) nil "-T" "-u" module)
            (goto-char (point-min))
            (when (and (> (count-lines (point-min) (point-max)) 1)
                       (not (re-search-forward
                             "No documentation found for .*" nil t)))
              (pod-linkable-sections-for-buffer (current-buffer)))))
      (kill-buffer (current-buffer)))))

(defun pod-linkable-sections (&optional module)
  (if module
      (pod-linkable-sections-for-module module)
    (pod-linkable-sections-for-buffer
     (current-buffer)
     (mapcar (lambda (i) (car i))
             pod-weaver-section-keywords))))

(defun pod-linkable-modules (&optional re-cache)
  (when (ignore-errors (require 'perldoc))
    (when (or re-cache (not perldoc-modules-alist))
      (message "Building completion list of all perl modules..."))
    (perldoc-modules-alist re-cache)))

(defun pod-link (link &optional text)
  (insert (concat "L<"
                  (when (and (stringp text)
                             (string-match-p "[^\s]" text))
                    (concat text "|"))
                  link
                  ">")))

(defun pod-link-uri (uri &optional text)
  (interactive
   (list (read-string "URI: ")
         (read-string "Text: ")))
  (pod-link uri text))

(defun pod-link-section (section &optional text)
  (interactive
   (list (completing-read "Section: " (pod-linkable-sections) nil nil)
         (read-string "Text: ")))
  (pod-link-module-section "" section text))

(defun pod-link-module (module &optional text)
  (interactive
   (list (completing-read "Module: "
                          (pod-linkable-modules current-prefix-arg) nil nil)
         (read-string "Text: ")))
  (pod-link module text))

(defun pod-link-module-section (module section &optional text)
  (interactive
   (let ((module (completing-read "Module: "
                                  (pod-linkable-modules current-prefix-arg)
                                  nil nil)))
   (list module
         (completing-read "Section: " (pod-linkable-sections module) nil nil)
         (read-string "Text: "))))
  (pod-link
   (concat module
           "/"
           (if (string-match-p "\s" section)
               (concat "\"" section "\"")
             section))
   text))

;; keymap
(defvar pod-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l u") 'pod-link-uri)
    (define-key map (kbd "C-c C-l s") 'pod-link-section)
    (define-key map (kbd "C-c C-l m") 'pod-link-module)
    (define-key map (kbd "C-c C-l M") 'pod-link-module-section)
    map)
  "Keymap for POD major mode.")

;; no special syntax table
(defvar pod-mode-syntax-table nil
  "Syntax table for `pod-mode'.")

;; create and activate syntax table
(defun pod-create-syntax-table ()
  (when (not pod-mode-syntax-table)
    (setq pod-mode-syntax-table (make-syntax-table))
    (set-syntax-table pod-mode-syntax-table)
    ))

(defun pod-add-support-for-outline-minor-mode ()
  "Provides additional menus from =head lines in `outline-minor-mode'."
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "=head[1-4]\s")
  (make-local-variable 'outline-level)
  (setq outline-level
        (function
         (lambda ()
           (save-excursion
             (save-match-data
               (if (looking-at
                    (concat "^="
                            (regexp-opt
                             (mapcar (lambda (i) (car i))
                                     pod-weaver-section-keywords) t)
                            "\s"))
                   (cdr (assoc (match-string-no-properties 1)
                               pod-weaver-section-keywords))
                 (string-to-int (buffer-substring
                                 (+ (point) 5)
                                 (+ (point) 6)))))))))

(defun pod-enable-weaver-collector-keywords (collectors)
  (let ((collectors-by-replacement))
    (save-match-data
      (setf pod-weaver-section-keywords
            (loop for col in collectors
                  with cmd with new-cmd with new-name
                  do (progn
                       (setq cmd (getf col 'command)
                             new-cmd (getf col 'new_command)
                             new-name (symbol-name new-cmd))
                       (let ((pos (loop for i in collectors-by-replacement do
                                        (when (equal (car i) new-cmd)
                                          (return i)))))
                         (if (not pos)
                             (push (list new-cmd cmd) collectors-by-replacement)
                           (setcdr (last pos) (list cmd)))))
                  when (string-match "^head\\([1-4]\\)$" new-name)
                  collect (cons (symbol-name cmd)
                                (string-to-int
                                 (match-string-no-properties 1 new-name)))))
      (let ((section-regexp
             (concat "="
                     (regexp-opt
                      (append
                       (mapcar (lambda (i) (car i))
                               pod-weaver-section-keywords)
                       (loop for i from 1 to 4
                             collect (concat "head" (int-to-string i)))))
                     "\s+")))
        (setf outline-regexp section-regexp)
        (setf imenu-generic-expression
              `((nil ,(concat "^" section-regexp "\\(.*\\)") 1))))
      (setf
       pod-font-lock-keywords
       (append
        pod-font-lock-keywords
        (mapcar (lambda (i)
                  (append
                   (list (concat
                          "^\\(="
                          (regexp-opt (mapcar (lambda (k) (symbol-name k))
                                              (cdr i)))
                          "\\)\\(.*\\)"))
                   (let ((n (symbol-name (car i))))
                     (if (string-match-p "^head[1-4]$" n)
                         (list
                          `(1 (quote
                               ,(intern (format "pod-mode-%s-face" n))))
                          `(2 (quote
                               ,(intern (format "pod-mode-%s-text-face" n)))))
                       (list
                        '(1 'pod-mode-command-face)
                        '(2 'pod-mode-command-text-face))))))
                collectors-by-replacement))))
    (setq font-lock-mode-major-mode nil)
    (font-lock-fontify-buffer))))

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
  (setq major-mode 'pod-mode)
  (setq mode-name "POD")
  (setq imenu-generic-expression '((nil "^=head[1-4] +\\(.*\\)" 1)))
  (run-hooks 'pod-mode-hook)
  (pod-add-support-for-outline-minor-mode)
  (pod-add-support-for-weaver)
  )

(provide 'pod-mode)

;;; pod-mode.el ends here
