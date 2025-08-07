;; Passing -1 will disable the toolbars along the top of the screen.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dark color theme
(load-theme 'wombat t)

;; We don't need the *~ files since almost everything should be backed up by git
(setq make-backup-files nil)

;; Place to drop manually installed packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(use-package markdown-mode
  :config
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)
  :mode ("README\\.md\\'" . gfm-mode)
  :defer t)

(use-package magit
  :ensure t
  :hook (git-commit-mode . (setq fill-column 72)))
