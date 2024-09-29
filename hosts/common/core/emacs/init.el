;; Laad Org-agenda configuratie
;;(load-file "~/.emacs.d/org-agenda.el")

;; Laad Org-present configuratie
;;(load-file "~/.emacs.d/org-present.el")

;Basisinstellingen voor een betere look

;; Zorg ervoor dat Emacs een bepaald frame laat zien
(add-hook 'emacs-startup-hook
          (lambda ()
            (delete-other-windows)))

;; Verwijder de fringe
(fringe-mode 0)

;; Verwijder eventuele achtergrondkleur van de fringes (werkt soms beter met transparantie)
(set-face-attribute 'fringe nil :background nil)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(recentf-mode 1)

;; Bewaar minibuffer geschiedenis
(setq history-length 25)
(savehist-mode 1)  ; Bewaart minibuffer geschiedenis tussen Emacs sessies

;; Onthoud en herstel de cursor positie van eerder geopende bestanden
(save-place-mode 1)

;; Automatisch herladen van bestanden die buiten Emacs zijn gewijzigd
(global-auto-revert-mode 1)  ; Essentieel voor Syncthing synchronisatie
(setq auto-revert-interval 1)  ; Controleer elke seconde op bestandswijzigingen
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(evil-mode 1)

;; Open de Org-agenda bij opstarten
(add-hook 'emacs-startup-hook 'org-agenda-list)

;; Voeg alle .org-bestanden in de "Sync/Org" directory toe aan de Org-agenda
(setq org-agenda-files (directory-files-recursively "~/Sync/Org" "\\.org$"))


;; Function to be run when org-agenda is opened
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

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
(require 'org-super-agenda)
(org-super-agenda-mode t)

;; From the official librephoenix config
(setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name " Overdue "  ; Optionally specify section name
                :scheduled past
                :order 2
                :face 'error)

         (:name "Personal "
                :and(:file-path "Personal.p" :not (:tag "event"))
                :order 3)

         (:name "Family "
                :and(:file-path "Family.s" :not (:tag "event"))
                :order 3)

         (:name "Teaching "
                :and(:file-path "Teaching.p" :not (:tag "event"))
                :order 3)

         (:name "Gamedev "
                :and(:file-path "Gamedev.s" :not (:tag "event"))
                :order 3)

         (:name "Youtube "
                :and(:file-path "Producer.p" :not (:tag "event"))
                :order 3)

         (:name "Music "
                :and(:file-path "Bard.p" :not (:tag "event"))
                :order 3)

         (:name "Storywriting "
                :and(:file-path "Stories.s" :not (:tag "event"))
                :order 3)

         (:name "Writing "
                :and(:file-path "Author.p" :not (:tag "event"))
                :order 3)

         (:name "Learning "
                :and(:file-path "Knowledge.p" :not (:tag "event"))
                :order 3)

          (:name " Today "  ; Optionally specify section name
                :time-grid t
                :date today
                :scheduled today
                :order 1
                :face 'warning)

;; Load org-modern
(with-eval-after-load 'org (global-org-modern-mode))
