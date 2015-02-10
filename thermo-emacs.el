
;specifify the custom file
(setq custom-file (expand-file-name "emacs-custom.el" thermo-emacs-dir))
(load custom-file)

; add init directory to load path
(add-to-list 'load-path (expand-file-name "lisp" thermo-emacs-dir))

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;disable tollbar
(tool-bar-mode -1)

; line numbers
(global-linum-mode 1)

; ido-mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; make newline automatically ident
(define-key global-map (kbd "RET") 'newline-and-indent)

(add-to-list 'load-path "~/.emacs.d/markdown-mode/")
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(markdown-enable-math)

; diables dialog boxes on OS X, which are completely broke
; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


;; visual line mode breakes the lines at the window edges, but only visually -- the buffer displays as two lines, but there is only one "logical line"
(global-visual-line-mode 1)

;; commands suggested by the "Writing GNU Emacs Extensions" book

(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)

                                        ; OS-specific

(add-to-list 'load-path "~/.emacs.d/deft/")
(require 'deft)

(setq deft-extensions '("txt" "org" "taskpaper" "md"))
(setq deft-default-extension "md")
(setq deft-directory "~/Dropbox/notes")
(setq deft-text-mode 'markdown-mode)
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)

(eval-after-load "deft"
'(define-key deft-mode-map (kbd "\C-c j") 'deft-new-file-named))


;---
;;loading ELPA packages
(setq package-enable-at-startup nil)
(package-initialize)

(load-theme 'solarized-light 1)

(if (eq system-type 'darwin)
    (progn
      (exec-path-from-shell-initialize)
      (set-variable 'TeX-view-program-selection
                      '((output-pdf "Preview")))
      )
  (if (eq system-type 'windows-nt)
      (progn
        (set-face-font 'default "InputMono")
        ;; Make sure that the bash executable can be found
        (setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")
        (setq shell-file-name explicit-shell-file-name)
        (add-to-list 'exec-path "C:/cygwin64/bin")
        (set-variable 'TeX-view-program-selection
                      '((output-pdf "Sumatra")))
        (setq ahk-syntax-directory "C:/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax")
        (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
        (autoload 'ahk-mode "ahk-mode")

        )
    )
  )


(require 'org)

;; Make RefTeX work with Org-Mode
;; use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
;; source: http://orgmode.org/worg/org-faq.html#using-reftex-in-org-mode

(defun org-mode-reftex-setup ()
  (load-library "reftex") 
  (and (buffer-file-name)
  (file-exists-p (buffer-file-name))
  (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

; enable markdown exporter
; source: http://stackoverflow.com/questions/22988092/emacs-org-mode-export-markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; some nice AUCTEX configurations
;; source: http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;; AucTeX

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(eval-after-load "latex"
'(define-key docTeX-mode-map (kbd "\C-c j") 'comment-indent-new-line))

;; ;; Use Skim as viewer, enable source <-> PDF sync
;; ;; make latexmk available via C-c C-c
;; ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (push
;;     '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;       :help "Run latexmk on file")
;;     TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
;; ;; use Skim as default pdf viewer
;; ;; Skim's displayline is used for forward search (from .tex to .pdf)
;; ;; option -b highlights the current line; option -g opens Skim in the background  
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


; setup Reftex and enable in Markdown mode
(require 'reftex)
(add-hook 'markdown-mode-hook 'turn-on-reftex)

; enable reftex in LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

; setup pandoc mode
(require 'pandoc-mode)

(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(require 'bookmark+)

(put 'upcase-region 'disabled nil)


