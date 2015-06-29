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