;;; org-agenda.el --- Org agenda configuration

;;; Commentary:
;; This file contains settings specific to Org agenda views and functionality,
;; including custom commands and appearance settings.

;;; Code:

;; Org Agenda Configuration

;; This file contains settings specific to Org agenda views and functionality.

;; Add all .org files in the "Sync/Org" directory to Org-agenda
(setq org-agenda-files (directory-files-recursively "~/Sync/Org" "\\.org$"))

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

;; Add side spacing to the agenda view
(setq org-agenda-prefix-format
      '((agenda . "  %-12:c%?-12t% s")
        (todo . "  %-12:c")
        (tags . "  %-12:c")
        (search . "  %-12:c")))

;; Increase left margin for better readability
(setq org-agenda-indent-level 2)

;; Set a reasonable width for the agenda view
(setq org-agenda-tags-column -80)

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

;; Function to apply custom agenda styling
(defun my/org-agenda-style-hook ()
  "Hook to apply custom styling to org-agenda buffers."
  (setq-local line-spacing 0.2)  ; Adjust line spacing
  (setq-local left-margin-width 2)  ; Add left margin
  (setq-local right-margin-width 2)  ; Add right margin
  (set-window-buffer nil (current-buffer)))  ; Apply changes

;; Add the hook to Org-agenda mode
(add-hook 'org-agenda-mode-hook 'my/org-agenda-style-hook)

(provide 'org-agenda)
;;; org-agenda.el ends here