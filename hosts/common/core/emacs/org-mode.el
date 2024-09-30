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

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Sync/Org/todos.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Sync/Org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

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

(provide 'org-mode)
;;; org-mode.el ends here