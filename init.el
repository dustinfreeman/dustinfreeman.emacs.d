;; Dustin Freeman's .emacs.d/init.el
;; Created: Aug 21, 2018

(load-theme 'misterioso)

;; Melpa install
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-git-gutter-mode t)
 '(package-selected-packages
   (quote
    (magit fireplace circe tide prettier-js rust-mode git-gutter dockerfile-mode docker auto-complete-clang jedi ## persistent-scratch markdown-mode shx omnisharp csv-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; I'd love to have the bash shell hosting emacs, and emacs M-x shell, to
;; share history in .zsh_history or similar. This may do that.
(savehist-mode 1)

;; Most used for when discarding changes in git shell
(global-auto-revert-mode)

;; for gcloud ssh
;; https://qiita.com/tanatana/items/218b19808f2428b125fe
(require 'tramp)
(add-to-list 'tramp-methods
  '("gcssh"
    (tramp-login-program        "gcloud compute ssh")
    (tramp-login-args           (("%h")))
    (tramp-async-args           (("-q")))
    (tramp-remote-shell         "/bin/sh")
    (tramp-remote-shell-args    ("-c"))
    (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                 ("-o" "UserKnownHostsFile=/dev/null")
                                 ("-o" "StrictHostKeyChecking=no")))
    (tramp-default-port         22)))

;; autocomplete
(require 'auto-complete)
(global-auto-complete-mode t)

;; annotations that show changed lines for git-tracked files
(require 'git-gutter)
(global-git-gutter-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;; rust autoformat
;; must must install: $ rustup component add rustfmt-preview
(setq rust-format-on-save t)

;; javascript autoformat 
;; must install; $ npm install -g prettier
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)

;; magit settings
(global-set-key (kbd "C-x g") 'magit-status)
