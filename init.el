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
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Place to drop manually installed packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Set the line break to 100 chars (default is 70)
(setq fill-column 100)

;; Ensure correct amount of whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Don't ring the terminal bell on any errors, it's super annoying
(setq ring-bell-function 'ignore)

(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Even though we're using Wayland, window-system still exports 'x on Linux
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; Keybindings
(global-set-key (kbd "C-c C-,") 'comment-region)

;; On macOS my Emacs treesitter library has ABI version 14, but on Linux it's 15. I need to use different grammar versions for each.
(defvar treesit-language-source-alist nil)
(let ((abi-version (treesit-library-abi-version)))
  (setq treesit-language-source-alist
        (cond
         ;; ABI version 14 (tree-sitter 0.20.x)
         ((= abi-version 14)
	  '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	    (json "https://github.com/tree-sitter/tree-sitter-json")
            (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

         ;; Default to latest for unknown versions
         (t
	  '((c "https://github.com/tree-sitter/tree-sitter-c")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	    (json "https://github.com/tree-sitter/tree-sitter-json")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
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
