; Basisinstellingen voor een betere look

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


