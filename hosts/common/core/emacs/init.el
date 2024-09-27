; Basic settings to make it look gewd

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(recentf-mode 1)

(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)
(global-auto-revert-mode 1)


(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(evil-mode 1)

;; Org mode TAB cycling in both normal and insert modes
(defun my/org-mode-setup ()
  "Custom configurations for Org mode."
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "TAB") 'org-cycle))

(add-hook 'org-mode-hook 'my/org-mode-setup)


