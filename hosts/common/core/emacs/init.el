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

;; Fringe settings (the vertical bars at the side of the window)
(fringe-mode 0)                   ; Disable the fringe
(set-face-attribute 'fringe nil :background 'unspecified)

;; Transparency settings for the Emacs frame
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; Enable Evil mode for Vim-like keybindings
(evil-mode 1)

;; Enable global auto-revert mode for automatic file updates
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)                     ; Check for file changes every second
(setq global-auto-revert-non-file-buffers t)      ; Also auto-revert buffers like dired
(setq auto-revert-verbose nil)                    ; Don't show messages for every revert

;; Load custom keybindings
(load-file "~/.emacs.d/keybindings.el")

;; Load Org-mode configuration
(load-file "~/.emacs.d/org-mode.el")

;; Load Org-present configuration for presentations
(load-file "~/.emacs.d/org-present.el")

;; Load Org-modern for enhanced Org-mode visuals
(with-eval-after-load 'org (global-org-modern-mode))

;; Doom Modeline Configuration
(with-eval-after-load 'doom-modeline
  (doom-modeline-mode 1)
  ;; Optional configurations:
  (setq doom-modeline-height 15)                  ; Set the height of the modeline
  (setq doom-modeline-icon t)                     ; Display icons
  (setq doom-modeline-major-mode-icon t)          ; Display the major mode icon
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project))  ; Truncate file path

;; Initialize doom-modeline
(doom-modeline-mode 1)

;; All The Icons Configuration
(require 'all-the-icons)

;; Check if fonts are installed, if not, prompt to install them
(unless (find-font (font-spec :name "all-the-icons"))
  (message "all-the-icons fonts are not installed. Please run M-x all-the-icons-install-fonts"))

;; Function to apply fringe settings after theme load
(defun my/apply-fringe-settings ()
  "Apply fringe settings after theme load or init."
  (fringe-mode 0)
  (set-face-attribute 'fringe nil :background 'unspecified))

(add-hook 'after-init-hook 'my/apply-fringe-settings)
(add-hook 'after-load-theme-hook 'my/apply-fringe-settings)

;; Define the startup function to show the agenda
(defun my/startup-with-agenda ()
  "Open the agenda view on startup and remove other windows."
  (org-agenda nil "a")
  (delete-other-windows)
  (get-buffer "*Org Agenda*"))

;; Set the initial buffer choice to our custom startup function
(setq initial-buffer-choice 'my/startup-with-agenda)

;; Keep the *scratch* buffer but don't show it on startup
(setq initial-scratch-message nil)

(message "Init.el successfully loaded.")
(provide 'init)
;;; init.el ends here