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
  :ensure t
  :config
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)
  :mode ("README\\.md\\'" . gfm-mode)
  :defer t)

(use-package magit
  :ensure t
  :hook
  (git-commit-mode . (lambda () (setq fill-column 72))))

(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
