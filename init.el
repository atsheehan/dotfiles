;; Passing -1 will disable the toolbars along the top of the screen.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dark color theme
(load-theme 'wombat t)

;; We don't need the *~ files since almost everything should be backed up by git
(setq make-backup-files nil)

;; MELPA contains lsp-mode that isn't in ELPA
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Place to drop manually installed packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Set the PATH for running external processes
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do)
  :mode ("README\\.md\\'" . gfm-mode))

(use-package magit
  :ensure t
  :hook
  (git-commit-mode . (lambda () (setq fill-column 72))))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package lsp-mode
  :ensure t
  :defer t
  :hook (rust-mode . lsp))

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(json "https://github.com/tree-sitter/tree-sitter-json")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

;; When attempting to use c-mode, use c-ts-mode instead
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
	(cpp-mode . cpp-ts-mode)))

;; These languages don't have a built-in mode to replace, so just associate the extension with the new tree-sitter mode.
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
