;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; My Emacs configuration.

;;; Code:

;; Passing -1 will disable the toolbars along the top of the screen.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Always use spaces instead of tabs
(indent-tabs-mode -1)

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

;; Set the line break to 100 chars (default is 70)
(setq fill-column 100)

;; Ensure correct amount of whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Don't ring the terminal bell on any errors, it's super annoying
(setq ring-bell-function 'ignore)

(use-package markdown-mode
  :ensure t
  :defer t
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do))
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
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (rust-mode . lsp))

;; Temporary fix to disable a warning with rust-analyzer until the following is released in the next lsp version
;; https://github.com/emacs-lsp/lsp-mode/commit/dc75f2ad9fb7e516be30585653fd40452e752441
(defvar lsp-rust-analyzer-cargo-extra-env nil)
(with-eval-after-load 'lsp-rust
  (setq lsp-rust-analyzer-cargo-extra-env #s(hash-table)))

;; Displays error details on the sideline
(use-package lsp-ui
  :ensure t
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c d" . lsp-ui-doc-toggle)))

;; Completion dialogs in buffers
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; Provides inline syntax-checking
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package vertico
  :ensure t
  :init
  ;; Calling vertico in init will force the initialization on startup,
  ;; rather than lazy loading with :config
  (vertico-mode))

;; Highlight uncommitted changes in the fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; This variable will be used by the treesitter library when it's loaded
(defvar treesit-language-source-alist nil)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(json "https://github.com/tree-sitter/tree-sitter-json")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

;; When attempting to use c-mode, use c-ts-mode instead
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
	(cpp-mode . cpp-ts-mode)))

;; These languages don't have a built-in mode to replace, so just associate the extension with the new tree-sitter mode.
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

(provide 'init)
;;; init.el ends here
