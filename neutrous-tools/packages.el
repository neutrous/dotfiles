;;; packages.el --- multi-term Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 neutrous
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq neutrous-tools-packages
      '(
        multi-term
        compile
        jsx-mode
        ace-jump-mode
        gotest
        mustache-mode
        protobuf-mode
        org-mode
        ))

(defun neutrous-tools/init-multi-term()
  (use-package multi-term
    :config
    (progn

      (defun neutrous-tools//list-terminal-buffers ()
        (let ((result ()) (current-buffers (buffer-list)))
          (dolist (elem current-buffers result)
            (let ((bufname (buffer-name elem)))
              (when (or (string-prefix-p "*termi" bufname) (string-prefix-p "*multi" bufname))
                (setq result (cons (buffer-name elem) result)))))))

      (defun neutrous-tools//terminal-sources (terminals)
        "return a source for terminal selection"
        `((name . "Available Terminals")
          (candidates . ,(mapcar (lambda (terminal)
                                   (cons terminal terminal))
                                 terminals))
          (action . (lambda (candidate) (switch-to-buffer candidate)))))

      (defun neutrous-tools/terms-select ()
        "list currently available multi term buffers"
        (interactive)
        (helm :sources (list (neutrous-tools//terminal-sources
                              (neutrous-tools//list-terminal-buffers)))))

      ;; handy for create a new multi-term buffer
      (evil-leader/set-key
        "om" 'multi-term
        "ol" 'neutrous-tools/terms-select)

      (defun multi-term/set-term-configuration ()
        (setq term-buffer-maximum-size 10000)
        ;; Prefer to using zsh as the default shell
        (setq multi-term-program "/bin/zsh")
        (setq show-trailing-whitespace nil)
        ;; see https://github.com/syl20bnr/spacemacs/issues/2775 from color face problem solution.
        (setq system-uses-terminfo nil)
        (custom-set-faces
         '(term ((t (:inherit default)))))
        (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
        (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next)))

      (add-hook 'term-mode-hook 'multi-term/set-term-configuration)
      )))

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(defun neutrous-tools/init-compile ()
  (use-package compile
    :config
    (progn

      ;; refrences from prelude
      
      ;; Compilation from Emacs
      (defun prelude-colorize-compilation-buffer ()
        "Colorize a compilation mode buffer."
        (interactive)
        ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
        (when (eq major-mode 'compilation-mode)
          (let ((inhibit-read-only t))
            (ansi-color-apply-on-region (point-min) (point-max)))))

      (setq compilation-ask-about-save nil  ; Just save before compiling
            compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
            compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
            )

      (add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)
      )
    ))

(defun neutrous-tools/init-jsx-mode ()
  (use-package jsx-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
      ;; force tern work with jsx-mode
      (add-hook 'jsx-mode-hook (lambda() (tern-mode t)))
      )
    ))

(defun neutrous-tools/init-ace-jump-mode()
  (use-package ace-jump-mode
    :config
    (progn
      (define-key evil-normal-state-map "f" 'ace-jump-char-mode))))

(defun neutrous-tools/init-gotest()
  (use-package gotest
    :config
    (evil-leader/set-key-for-mode 'go-mode
      "mta" 'go-test-current-project
      "mtm" 'go-test-current-file
      "mt." 'go-test-current-test)))

(defun neutrous-tools/init-mustache-mode()
  (use-package mustache-mode))

(defun neutrous-tools/init-protobuf-mode()
  (use-package protobuf-mode))

(defun neutrous-tools/init-org-mode()
  (use-package org-mode
    :config
    (progn
      ;; settings come from http://doc.norang.ca/org-mode.html

      ;; settings for agenda
      (setq org-agenda-files (quote ("~/git/org"
                                     "~/git/org/client1"
                                     "~/git/client2")))
      )))

