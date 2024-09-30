;;; org-mode.el --- Org mode configuration

;;; Commentary:
;; This file contains settings for Org mode, including todo keywords, capture templates,
;; and visual settings.

;;; Code:

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

;; Load Org-agenda configuration
(load-file "~/.emacs.d/org-agenda.el")

;; Function to toggle Org prettify
(defun my/toggle-org-prettify ()
  "Toggle between pretty and raw Org mode view."
  (interactive)
  (if (bound-and-true-p org-pretty-entities)
      (org-mode-restart)
    (org-toggle-pretty-entities)))

;; Set up hooks for prettifying Org mode
(defun my/org-mode-prettify-hooks ()
  "Set up hooks for prettifying Org mode."
  (add-hook 'evil-normal-state-entry-hook 'org-toggle-pretty-entities nil t)
  (add-hook 'evil-insert-state-entry-hook (lambda () (org-toggle-pretty-entities -1)) nil t))

(add-hook 'org-mode-hook 'my/org-mode-prettify-hooks)

;; Function to toggle org-modern
(defun my/toggle-org-modern ()
  "Toggle org-modern on and off."
  (interactive)
  (if (bound-and-true-p org-modern-mode)
      (progn
        (org-modern-mode -1)
        (org-indent-mode -1)
        (olivetti-mode -1)
        (setq org-hide-leading-stars nil)
        (message "org-modern disabled"))
    (progn
      (org-modern-mode 1)
      (org-indent-mode 1)
      (olivetti-mode 1)
      (olivetti-set-width 0.7)  ; Set width to 70% of the window
      (setq org-hide-leading-stars t)
      (message "org-modern enabled")))
  (font-lock-fontify-buffer))

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
)

;; Function to adjust org appearance
(defun my/adjust-org-appearance ()
  (setq org-hide-leading-stars (bound-and-true-p org-modern-mode))
  (org-indent-mode (if (bound-and-true-p org-modern-mode) 1 -1))
  (olivetti-mode (if (bound-and-true-p org-modern-mode) 1 -1))
  (when (bound-and-true-p org-modern-mode)
    (olivetti-set-width 0.7))
  (font-lock-fontify-buffer))

;; Add hook to adjust appearance when org-modern is toggled
(add-hook 'org-modern-mode-hook 'my/adjust-org-appearance)

;; Enable org-modern by default
(add-hook 'org-mode-hook 'org-modern-mode)

(provide 'org-mode)
;;; org-mode.el ends here