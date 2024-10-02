;;; org-agenda.el --- Org agenda configuration

;;; Commentary:
;; This file contains settings specific to Org agenda views and functionality,
;; including custom commands and appearance settings.

;;; Code:

;; Org Agenda Configuration

;; This file contains settings specific to Org agenda views and functionality.

;; Add all .org files in the "Sync/Org" directory to Org-agenda
(setq org-agenda-files '("~/Sync/Org/Personal.org"
                         "~/Sync/Org/Projects.org"
                         "~/Sync/Org/Work.org"))

;; Hide duplicates of the same todo items
(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-timestamp-if-deadline-is-shown t)

;; Customize Org-agenda appearance
(setq org-agenda-current-time-string "")
(setq org-agenda-time-grid '((daily) () "" ""))

;; Customize the agenda view
(setq org-agenda-block-separator nil
      org-agenda-compact-blocks t)

;; Function to apply custom agenda styling
(defun my/org-agenda-style-hook ()
  "Hook to apply custom styling to org-agenda buffers."
  (setq-local line-spacing 0.2)  ; Adjust line spacing
  (setq-local left-margin-width 2)  ; Add left margin
  (setq-local right-margin-width 2)  ; Add right margin
  (set-window-buffer nil (current-buffer)))  ; Apply changes

;; Add the hook to Org-agenda mode
(add-hook 'org-agenda-mode-hook 'my/org-agenda-style-hook)

;; Add Olivetti width globally for all Org modes
(add-hook 'org-agenda-mode-hook 'my/set-global-olivetti-width)

(provide 'org-agenda)
;;; org-agenda.el ends here
