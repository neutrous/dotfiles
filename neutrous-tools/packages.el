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
        web-mode
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

(defun neutrous-tools/init-web-mode()
  (use-package web-mode
    :config
    (progn
      (defconst prettydiff-program "prettydiff")

      (defun neutrous-tools//prettydiff-format-region (program beg end)
        "By PROGRAM, format each line in the BEG .. END region."
        (if (executable-find program)
            (save-excursion
              (apply 'call-process-region beg end program t
                     (list t nil) t prettydiff-args))
          (message (web-beautify-command-not-found-message program))))

      (defun neutrous-tools//prettydiff-format-buffer (program extenstion)
        "By PROGRAM, format current buffer with EXTENSTION."
        (if (executable-find program)
            (neutrous-tools//prettydiff-format-buffer-1 program extenstion)
          (message (web-beautify-command-not-found-message program))))

      (defun neutrous-tools//prettydiff-format-buffer-1 (program extenstion)
        "Internal function of `web-beautify-format-buffer'.

By PROGRAM, format current buffer with EXTENSTION."
        (let* ((tmpfile (make-temp-file "prettydiff-beautify" nil
                                        (format ".%s" extenstion)))
               (outputbufname (format "*prettydiff-beautify-%s*" extenstion))
               (outputbuf (get-buffer-create outputbufname))
               (args `("mode:beautify" "insize:2" ,(format "source:%s" tmpfile))))
          (unwind-protect
              (progn
                (with-current-buffer outputbuf (erase-buffer))
                (write-region nil nil tmpfile)

                (if (zerop (apply 'call-process program nil outputbuf nil args))
                    (let ((p (point)))
                      (save-excursion
                        (with-current-buffer (current-buffer)
                          (erase-buffer)
                          (insert-buffer-substring outputbuf)))
                      (goto-char p)
                      (message "Applied prettydiff-beautify")
                      (kill-buffer outputbuf))
                  (message (web-beautify-format-error-message outputbufname))
                  (display-buffer outputbuf)))
            (progn
              (delete-file tmpfile)))))

      (defun neutrous-tools/prettydiff ()
        "Format region if active, otherwise the current buffer.

Formatting is done according to the prettyidff command if exist."
        (interactive)
        (if (use-region-p)
            (neutrous-tools//prettydiff-format-region
             prettydiff-program
             (region-beginning) (region-end))
          (neutrous-tools//prettydiff-format-buffer prettydiff-program "html")))
      )))
