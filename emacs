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
(use-package scss-mode
  :init
  (setq scss-compile-at-save nil))

(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package helm-ls-git
  :bind ("C-x C-d" . 'helm-browse-project))

(use-package web-mode
  :init
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package magit)
(use-package json-mode)
(use-package git-gutter)
(use-package enh-ruby-mode
  :init
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-add-encoding-comment-on-save nil))

(use-package elixir-mode)

;; Remove unused toolbars to gain more screen real estate
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Ensure correct amount of whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(load-theme 'wombat t)

(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-git-gutter-mode 1)

(global-set-key (kbd "C-c C-,") 'comment-region)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'replace-string)

(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
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

(setq org-log-done t)

(setq ring-bell-function 'ignore)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq create-lockfiles nil)
(setq ruby-insert-encoding-magic-comment nil)

(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))

(global-git-commit-mode)
(server-start)
