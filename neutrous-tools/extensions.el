(defun neutrous-tools//init-prettydiff ()
  "Using prettydiff to format html files."
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
    ))

(defun neutrous-tools//go-mode-extra ()
  (progn
    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    (defun spacemacs/go-run-package-tests ()
      (interactive)
      (shell-command "go test"))

    (evil-leader/set-key-for-mode 'go-mode
      "mtp" 'spacemacs/go-run-package-tests)))

(setq neutrous-tools-pre-extension
      '(
        ))

(setq neutrous-tools-post-extension
      '(
        (setq dired-dwim-target t)
        (define-key evil-normal-state-map "f" 'ace-jump-char-mode)
        ))

(use-package go-mode
  :config
  (neutrous-tools//go-mode-extra))

(use-package web-mode
  :config
  (neutrous-tools//init-prettydiff))

(global-linum-mode)
