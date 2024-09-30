;;; org-agenda.el --- Org agenda configuration

;;; Commentary:
;; This file contains settings specific to Org agenda views and functionality,
;; including custom commands and appearance settings.

;;; Code:

;; Org Agenda Configuration

;; This file contains settings specific to Org agenda views and functionality.

;; Add all .org files in the "Sync/Org" directory to Org-agenda
(setq org-agenda-files (directory-files-recursively "~/Sync/Org" "\\.org$"))

;; Function to enable Olivetti mode when Org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to activate Olivetti mode when org-agenda is opened."
  (olivetti-mode))

;; Add the hook to Org-agenda mode
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

;; Set the agenda to show only one day at a time
(setq org-agenda-span 1
      org-agenda-start-day "+0d")

;; Hide duplicates of the same todo items
(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-timestamp-if-deadline-is-shown t)

;; Customize Org-agenda appearance
(setq org-agenda-current-time-string "")
(setq org-agenda-time-grid '((daily) () "" ""))

;; Remove category names and scheduling types from the agenda view
(setq org-agenda-prefix-format '(
  (agenda . "  %?-2i %t ")
  (todo . " %i %-12:c")
  (tags . " %i %-12:c")
  (search . " %i %-12:c")))

;; Custom agenda command for a weekly review
(setq org-agenda-custom-commands
      '(("W" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)))
          (stuck "")
          (todo "WAITING")
          (todo "IN-PROGRESS")))))

;; Set up agenda workflow states
(setq org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-deadlines 'far)

;; Customize the agenda view
(setq org-agenda-block-separator nil
      org-agenda-compact-blocks t)

(provide 'org-agenda)
;;; org-agenda.el ends here