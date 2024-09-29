;; Voeg alle .org-bestanden in de "Sync/Org" directory toe aan de Org-agenda
(setq org-agenda-files (directory-files-recursively "~/Sync/Org" "\\.org$"))


;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

;; Open de Org-agenda bij opstarten
(add-hook 'emacs-startup-hook 'org-agenda-list)



;; Adds hook to org agenda mode, making follow mode active in org agenda
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

;; Only show one day of the agenda at a time
(setq org-agenda-span 1
      org-agenda-start-day "+0d")

;; Hide duplicates of the same todo item
;; If it has more than one of timestamp, scheduled,
;; or deadline information
(setq org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-timestamp-if-deadline-is-shown t)

;; Ricing org agenda
(setq org-agenda-current-time-string "")
(setq org-agenda-time-grid '((daily) () "" ""))

;; Add icons!
(setq org-agenda-category-icon-alist
      `(("MWLabs.p" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
        ("Home.s" ,(list (all-the-icons-faicon "home" :v-adjust 0.005)) nil nil :ascent center)
        ("GS.p" ,(list (all-the-icons-faicon "youtube-play" :height 0.9)) nil nil :ascent center)
        ("Bard.p" ,(list (all-the-icons-faicon "music" :height 0.9)) nil nil :ascent center)
        ("Nixdev.s" ,(list (all-the-icons-faicon "gamepad" :height 0.9)) nil nil :ascent center)
        ("Knowledge.p" ,(list (all-the-icons-faicon "database" :height 0.8)) nil nil :ascent center)
        ("Personal.p" ,(list (all-the-icons-material "person" :height 0.9)) nil nil :ascent center)
))

;; Remove category names and scheduling type from agenda view
(setq org-agenda-prefix-format '(
(agenda . "  %?-2i %t ")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))

;; Load org-super-agenda

