(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default x-select-enable-clipboard t)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default jsx-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)
(setq-default ruby-deep-indent-paren nil)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.org/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-list
      '(yaml-mode
        haml-mode
        scss-mode
        sbt-mode
        scala-mode2
        markdown-mode
        helm-ls-git
        magit
        ensime
        elixir-mode
        git-gutter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-c C-,") 'comment-region)
(global-set-key (kbd "C-c f") 'fill-region)
(global-set-key (kbd "C-c p") 'fill-paragraph)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'replace-string)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avdl\\'" . java-mode))

(setq require-final-newline t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq scss-compile-at-save nil)

(set-face-attribute 'default nil :height 160)

(load-theme 'wombat t)

(global-git-gutter-mode +1)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

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
