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

;; Algemene Spacious Padding configuratie
(use-package spacious-padding
  :hook (after-init . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8
          :fringe-width 12)))  ; Iets grotere fringe-breedte

;; Functie om extra padding toe te voegen voor Org-mode
(defun my/org-mode-padding ()
  (setq left-margin-width 300
        right-margin-width 300)
  (set-window-buffer nil (current-buffer)))

;; Org-mode configuratie
(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t)
  (setq org-ellipsis "⤵")

  ;; Aangepaste lettergroottes voor koppen met grotere verschillen
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-level-6 ((t (:inherit outline-6 :height 1.0))))))

;; Professionele bullets en moderne look
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (setq org-modern-hide-stars nil)
  (setq org-modern-timestamp nil)
  (setq org-modern-table nil))

;; Functie om org-mode en gerelateerde modi in te stellen
(defun my/org-mode-setup ()
  "Aangepaste configuraties voor Org mode."
  (org-bullets-mode 1)
  (org-modern-mode 1)
  (visual-line-mode 1)
  (org-indent-mode 1)
  (setq left-margin-width 4
        right-margin-width 4)
  (set-window-buffer nil (current-buffer))
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "TAB") 'org-cycle))

;; Voeg deze hook toe om org-indent-mode globaal in te schakelen voor org-bestanden
(add-hook 'org-mode-hook 'org-indent-mode)

;; Configuratie voor betere tekstomloop in Org-mode
(setq org-startup-indented t)
(setq org-adapt-indentation t)

;; Globale instelling voor visuele lijnmodus
(global-visual-line-mode 1)

;; Extra padding voor Org-mode
(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 500
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

