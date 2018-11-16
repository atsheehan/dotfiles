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

(use-package yaml-mode)
(use-package scss-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package helm-ls-git)
(use-package web-mode)
(use-package magit)
(use-package json-mode)
(use-package git-gutter)
(use-package enh-ruby-mode)
(use-package elixir-mode)

;; Remove unused toolbars to gain more screen real estate
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Ensure correct amount of whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(load-theme 'wombat t)

(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default jsx-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)
(setq-default ruby-deep-indent-paren nil)

(global-git-gutter-mode +1)

(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-c C-,") 'comment-region)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'replace-string)

(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq scss-compile-at-save nil)

(setq web-mode-content-types-alist
      '(("jsx" . ".*\\.js[x]?\\'")))

(defun shallow-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'shallow-indent-setup)

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq org-log-done t)
(setq ruby-use-smie nil)

(setq ring-bell-function 'ignore)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq create-lockfiles nil)
(setq enh-ruby-deep-indent-paren nil)
(setq ruby-insert-encoding-magic-comment nil)

(server-start)
(global-git-commit-mode)

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))
