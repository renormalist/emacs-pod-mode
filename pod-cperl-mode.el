;;; pod-cperl-mode.el --- Editing Perl inside POD verbatim blocks

;; Copyright (C) 2003, 2007, 2009  Free Software Foundation, Inc.

;; Author: Steffen Schwigon
;; Frivolously stolen from haskell-latex-mode. 
;; Respective credits follow:

;; Author: Dave Love <fx@gnu.org>

;; Keywords: languages, wp
;; Created: 2010
;; $Revision: 0.1 $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a mode for editing Perl in verbatim blocks of POD.
;; 
;; You probably want to do something like this:
;; 
;;   (add-to-list 'auto-mode-alist '("\\.pod\\'" . pod-cperl-mode))
;;   (autoload 'pod-cperl-mode "pod-cperl")
;;

;;; Code:

(require 'multi-mode)
(defun pod-cperl-chunk-region (pos)
  "Determine type and limit of current chunk at POS."
    (let ((mode 'pod-mode)
          (start (point-min))
          (end (point-max))
          (info "none"))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char pos)
          (cond
           ;; inside verbatim
           ((save-excursion
              (beginning-of-line)
              (looking-at " "))
            (progn
              (setq info "--verbatim--")
              ;;(message info)
              (setq mode 'cperl-mode)
              (save-excursion
                (setq start (if (re-search-backward "^\\([^ ]\\)" nil t)
                                (progn (beginning-of-line 2) (point))
                              (point-min))
                      end (if (re-search-forward "^\\([^ ]\\)" nil t)
                              (progn (beginning-of-line) (backward-char) (backward-char) (point))
                            (point-max))))))
           ;; outside verbatim, assume pod
           (t
            (progn
              (setq info "--pod--")
              ;;(message info)
              (setq mode 'pod-mode)
              (save-excursion
                (setq start (if (re-search-backward "^\\([ ]\\)" nil t)
                                (progn (beginning-of-line 2) (point))
                              (point-min))
                      end (if (re-search-forward "^\\([ ]\\)" nil t)
                              (progn (beginning-of-line) (backward-char) (point))
                            (point-max)))))))
          ;;(message (concat info " " (prin1-to-string mode) ": " (prin1-to-string start) ".." (prin1-to-string end)))
          (multi-make-list mode start end)))))

;;;###autoload
(defun pod-cperl-mode ()
  "Mode for editing Perl inside POD verbatim blocks."
  (interactive)
  (set (make-local-variable 'multi-mode-alist)
       '((cperl-mode . pod-cperl-chunk-region)
	 (pod-mode . nil)))
  (multi-mode-install-modes))

(provide 'pod-cperl-mode)
;;; pod-cperl-mode.el ends here
