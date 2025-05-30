;;; init.el --- Emacs initialization file

;;; Commentary:
;; This file loads all necessary configurations and packages for a customized Emacs setup.

;;; Code:

;; Basic settings for a cleaner and more minimal Emacs interface
(setq inhibit-startup-message t)  ; Disable the startup message
(scroll-bar-mode -1)              ; Disable visible scrollbar
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(menu-bar-mode -1)                ; Disable the menu bar
(recentf-mode 1)                  ; Enable recent files mode

;; Lettergrootte-instellingen
(set-face-attribute 'default nil :height 140)  ; Standaard lettergrootte instellen op 14 punten
(set-face-attribute 'mode-line nil :height 120)  ; Modeline lettergrootte instellen op 12 punten

;; Fringe settings (the vertical bars at the side of the window)
(fringe-mode 0)                   ; Disable the fringe
(set-face-attribute 'fringe nil :background 'unspecified)

;; Transparency settings for the Emacs frame
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; Enable Evil mode for Vim-like keybindings
(evil-mode 1)

;; Vertico: Minimal minibuffer UI
(require 'vertico)
(vertico-mode)

;; Enable global auto-revert mode for automatic file updates
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)                     ; Check for file changes every second
(setq global-auto-revert-non-file-buffers t)      ; Also auto-revert buffers like dired
(setq auto-revert-verbose nil)                    ; Don't show messages for every revert

;; Load custom keybindings
(load-file "~/.emacs.d/keybindings.el")

;; Load Org-mode configuration
(load-file "~/.emacs.d/org-mode.el")

;; Load Org-agenda configuration
(load-file "~/.emacs.d/org-agenda.el")

;; Load Org-present configuration for presentations
(load-file "~/.emacs.d/org-present.el")

;; Load MWLabs invoice and offer generation system
(let ((mwlabs-init (expand-file-name "~/.emacs.d/mwlabs/init.el")))
  (if (file-exists-p mwlabs-init)
      (progn
        (message "Loading MWLabs system from: %s" mwlabs-init)
        (load-file mwlabs-init))
    (message "Warning: MWLabs init file not found at: %s" mwlabs-init)))

;; Doom Modeline Configuration
(with-eval-after-load 'doom-modeline
  (doom-modeline-mode 1)
  ;; Optional configurations:
  (setq doom-modeline-height 15)                  ; Set the height of the modeline
  (setq doom-modeline-icon t)                     ; Display icons
  (setq doom-modeline-major-mode-icon t)          ; Display the major mode icon
  (setq doom-modeline-buffer-file-name-style 'file-name))  ; Show the full file path

;; Initialize doom-modeline
(doom-modeline-mode 1)

;; All The Icons Configuration
(require 'all-the-icons)

;; Function to check and install all-the-icons fonts if needed
(defun ensure-all-the-icons-fonts ()
  "Check if all-the-icons fonts are installed, and install them if not."
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

;; Run the function to ensure fonts are installed
(with-eval-after-load 'all-the-icons
  (ensure-all-the-icons-fonts))

;; Function to apply fringe settings after theme load
(defun my/apply-fringe-settings ()
  "Apply fringe settings after theme load or init."
  (fringe-mode 0)
  (set-face-attribute 'fringe nil :background 'unspecified))

(add-hook 'after-init-hook 'my/apply-fringe-settings)
(add-hook 'after-load-theme-hook 'my/apply-fringe-settings)

;; Function to reload all NixOS Emacs configuration files
(defun reload-nixos-emacs-config ()
  "Reload all NixOS Emacs configuration files without restarting Emacs."
  (interactive)
  (let ((config-dir "~/Nixos-config/hosts/common/core/emacs/")
        (config-files '("init.el" "org-agenda.el" "org-mode.el" "org-present.el" "keybindings.el")))
    (dolist (file config-files)
      (let ((file-path (expand-file-name file config-dir)))
        (when (file-exists-p file-path)
          (load-file file-path))))
    (message "NixOS Emacs configuration reloaded successfully")))

;; Bind the function to F5 key
(global-set-key (kbd "<f5>") 'reload-nixos-emacs-config)

;; Disable auto-save
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Load MWLabs capture templates
(require 'mwlabs-init)

;; Set up org-capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

(message "Init.el successfully loaded.")
(provide 'init)
;;; init.el ends here
