(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-set-key (kbd "C-c C-,") 'comment-region)
(global-set-key (kbd "<f5>") 'replace-string)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default x-select-enable-clipboard t)
(setq-default column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default ruby-deep-indent-paren nil)
(setq-default js-indent-level 2)

(setq package-list
      '(yaml-mode
        haml-mode
        scss-mode
        scala-mode
        markdown-mode
        color-theme))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'color-theme)
(color-theme-billw)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

(setq require-final-newline t)
