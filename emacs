(require 'package)
(add-to-list 'package-archives
             '("melpa" .
               "https://melpa.org/packages/") t)
(package-initialize)

(if (not package-archive-contents)
    (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'use-package)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp))
  :hook ((c-mode . lsp))
  :commands lsp)
;; Taken from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(use-package yaml-mode)
(use-package scss-mode
  :init
  (setq scss-compile-at-save nil))

(use-package rustic)
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package foreman-mode)
(use-package rg
  :init
  (rg-enable-default-bindings))
(use-package graphql-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package helm-ls-git
  :bind ("C-x C-d" . 'helm-browse-project))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-f") 'foreman-start)

(use-package web-mode
  :mode
  ("\\.js\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.ts\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  ("\\.hbs\\'" . web-mode)
  ("\\.html\\'" . web-mode)
  ("\\.html.eex\\'" . web-mode)
  ("\\.html.erb\\'" . web-mode)
  :init
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'"))))

(push (concat (file-name-as-directory (getenv "HOME")) ".cargo/bin") exec-path)
(push (concat (file-name-as-directory (getenv "HOME")) ".rbenv/shims") exec-path)
(push (concat (file-name-as-directory (getenv "HOME")) "bin") exec-path)

(use-package magit
  :init
  (setq git-commit-fill-column 72))
(use-package json-mode
  :mode "\\.avsc\\'")
(use-package jsonnet-mode)

;; Display which lines have been modified since last commit. The
;; fringe package works with line numbers.
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1))

(use-package enh-ruby-mode
  :mode
  ("Gemfile" . enh-ruby-mode)
  ("Cheffile" . enh-ruby-mode)
  ("Rakefile" . enh-ruby-mode)
  ("\\.rb\\'" . enh-ruby-mode)
  ("\\.gemspec\\'" . enh-ruby-mode)
  ("\\.rake\\'" . enh-ruby-mode)
  :init
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-add-encoding-comment-on-save nil))

(use-package groovy-mode)
(use-package typescript-mode
  :init
  (setq typescript-indent-level 2))
(use-package elixir-mode)
(use-package toml-mode)
(use-package rust-mode)
(use-package glsl-mode)

(use-package company)

;; Remove unused toolbars to gain more screen real estate
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Global line numbers mode
(global-display-line-numbers-mode 1)

;; Hide splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Ensure correct amount of whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(load-theme 'wombat t)

(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-set-key (kbd "C-c C-,") 'comment-region)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'replace-string)

(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Increase line wrapping to 100 characters
(setq-default fill-column 100)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(setq ring-bell-function 'ignore)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq create-lockfiles nil)
(setq ruby-insert-encoding-magic-comment nil)

(global-git-commit-mode)
(server-start)

(setq org-refile-targets '(("~/org/salsify.org" :maxlevel . 2)
                           ("~/org/personal.org" :level . 1)
                           ("~/org/scheduled.org" :maxlevel . 2)
                           ("~/org/books.org" :maxlevel . 1)))

(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Monaco")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 165))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(wombat))
 '(lsp-rust-analyzer-cargo-watch-command "test")
 '(lsp-rust-analyzer-cargo-watch-enable t)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)))
 '(org-agenda-files '("~/org/scheduled.org" "~/org/salsify.org"))
 '(org-capture-templates
   '(("t" "Add TODO to inbox" entry
      (file+headline "~/org/inbox.org" "Inbox")
      (file "~/org/tpl-inbox.txt"))))
 '(org-export-backends '(ascii html icalendar latex md))
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-log-into-drawer t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-use-outline-path 'file)
 '(package-selected-packages
   '(rustic lua-mode treemacs lsp-java company robe robe-mode glsl-mode git-gutter-fringe+ rbenv lsp-mode sql-presto jsonnet-mode graphql-mode org-ref csv-mode typescript-mode groovy-mode rust-mode toml-mode yaml-mode web-mode use-package scss-mode markdown-mode magit json-mode helm-ls-git enh-ruby-mode elixir-mode dockerfile-mode))
 '(safe-local-variable-values '((whitespace-line-column . 80))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
