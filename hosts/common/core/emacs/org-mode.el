;;; org-mode.el --- Org mode configuration

;;; Commentary:
;; This file contains settings for Org mode, including todo keywords, capture templates,
;; and visual settings.

;;; Code:

;; Enable org-modern by default
(with-eval-after-load 'org
  (global-org-modern-mode))

;; Basic Org mode settings
(setq org-default-notes-file "~/Sync/Org/notes.org")
(setq org-startup-indented t)
(setq org-log-done 'time)

;; Todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Visual settings
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
(add-hook 'org-mode-hook 'visual-line-mode)

;; Set org-modern indicators and sizes
(setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
      org-modern-hide-stars 'leading
      org-modern-checkbox '((?X . "☑") (?- . "☐") (?\s . "☐"))
      org-modern-tag nil
      org-modern-priority nil
      org-modern-todo nil
      org-modern-timestamp nil
      org-modern-statistics nil
      org-modern-variable-pitch nil
      org-modern-block-fringe nil)

;; Set different sizes for headings with more pronounced differences
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.8 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight semi-bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3 :weight medium))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2 :weight normal))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0))))
 '(org-document-title ((t (:inherit outline-1 :height 2.0 :weight bold))))
)

;; Enable org-modern by default
(add-hook 'org-mode-hook 'org-modern-mode)

;; Function to set global Olivetti width
(defun my/set-global-olivetti-width ()
  "Set Olivetti width globally for all Org modes."
  (olivetti-mode 1)
  (olivetti-set-width 0.7))

(add-hook 'org-mode-hook 'my/set-global-olivetti-width)
(add-hook 'org-agenda-mode-hook 'my/set-global-olivetti-width)

;; Enable pretty entities by default
(setq org-pretty-entities t)

(provide 'org-mode)
;;; org-mode.el ends here

