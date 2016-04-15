; Thermo Emacs

; init.el --- Where all the magic begins

;; INITIAL SETTINGS

(setq thermo-emacs-init-file (or (buffer-file-name) load-file-name))

(setq dotfiles-dir (file-name-directory thermo-emacs-init-file))

;specifify the custom file
(setq custom-file (expand-file-name "emacs-custom.el" dotfiles-dir))
(load custom-file)

; add init directory to load path
(add-to-list 'load-path (expand-file-name "lisp" dotfiles-dir))

; some usual directories

(setq home-dir (expand-file-name "." (getenv "HOME")))

(setq dropbox-dir (expand-file-name "Dropbox" home-dir))

; set coding system
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)


; disable the useless toolbar
(tool-bar-mode -1)

; enable line numbers
(global-linum-mode 1)

; enable "logical" lines (`C-n` moves to the next "visible" line
(global-visual-line-mode 1)

; Package management
(require 'package)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(pandoc-mode
    yasnippet
    exec-path-from-shell
    auto-complete
    elpy
    yaml-mode
    auctex
    column-marker
    polymode
    ess
    ein
    helm
    helm-bibtex
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


; I like the theme solarized, and this manual installation is the best option to me
(add-to-list 'custom-theme-load-path (expand-file-name  "emacs-color-theme-solarized" dotfiles-dir))
(add-to-list 'custom-theme-load-path (expand-file-name  "zenburn-emacs" dotfiles-dir))

(load-theme 'zenburn t)

; right now I'm experimenting with zenburn

; enable upcase-region
(put 'upcase-region 'disabled nil)

; highlight the 80-th column
(require 'column-marker)


;; MINOR PACKAGES

; setup IDO mode
; [temporarily disabled to use helm]
;; (ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)

; enable the expand-region package
(add-to-list 'load-path (expand-file-name "expand-region.el" dotfiles-dir))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; KEYBINDINGS

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)


;; SNIPPETS

(require 'yasnippet)
(yas-global-mode 1)



;; NOTES

; markdown mode
(add-to-list 'load-path (expand-file-name "markdown-mode" dotfiles-dir))
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

(markdown-enable-math)

; taskpaper mode
(load-file  (expand-file-name  "taskpaper.el" (expand-file-name "tpemacs" dotfiles-dir)) )
 (require 'taskpaper-mode)
(add-to-list 'auto-mode-alist '("\\.taskpaper\\'" . taskpaper-mode))

; a macro to insert a markdown link snippet
(fset 'yas-link-region
   [?\C-w ?l ?i ?n ?k C-tab ?\C-y tab])

(define-key markdown-mode-map (kbd "C-c l") 'yas-link-region)

; a macrot to insert a template for my "Master Project List" file
(fset 'master-project-list
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 104 23 109 112 108 C-tab] 0 "%d")) arg)))

(define-key markdown-mode-map (kbd "C-c m") 'master-project-list)

; functions to enable list continuation in markdown-mode
(defun thermo-emacs-markdown-inside-list-p ()
      "Return t if point inside list item, nil if it is not."
     (if (markdown-cur-list-item-bounds) t nil))

(defun thermo-emacs-markdown-enter-key ()
      "If point is inside markdown list, insert new list item, otherwise handle RET according to value of markdown-indent-on-enter"
      (interactive)
      (if (thermo-emacs-markdown-inside-list-p)
          (markdown-insert-list-item 1)
        (if markdown-indent-on-enter (newline-and-indent) (newline))
    ))

(add-hook 'markdown-mode-hook
               (lambda ()
                 (define-key markdown-mode-map (kbd "<return>")
                             'thermo-emacs-markdown-enter-key)))

; deft-mode (notational-velocity)
(add-to-list 'load-path (expand-file-name "deft" dotfiles-dir))
(require 'deft)

(setq deft-extensions '("txt" "org" "taskpaper" "md"))
(setq deft-default-extension "md")
(setq deft-directory (expand-file-name "notes" dropbox-dir))
(setq deft-text-mode 'markdown-mode)

(setq deft-use-filename-as-title t)

; a "global" function to call the deft search box from anywhere
(defun thermo-emacs-deft-search ()
    "Open the deft buffer, clear the seach string and prompt for a new search string in minibuffer"
    (interactive)
    (deft)
    (deft-filter-clear)
    (call-interactively 'deft-filter)
)

(global-set-key (kbd "<f8>") 'thermo-emacs-deft-search)

; functions to manipulate my zettelaksten
(defun thermo-emacs-create-zettel ()
  "Prompt for a note title and automatically creates one in `deft-directory', with a zettel ID appended to the file name, and `deft-default-extension' appended"
  (interactive)

  (let (zettel-title zettel-id zettel-file-name)
  (setq zettel-title
        (read-string "Note title: "))

  (setq zettel-id (format-time-string "%Y%m%d%H%M%S"))

  (setq zettel-file-name 
	(expand-file-name (concat zettel-id " " zettel-title "." deft-default-extension) deft-directory))

  (find-file zettel-file-name)

  (insert
   (format
  "---
Title: %s  
Author: Fábio Fortkamp  
Date: %s  
Tags:  
bibliography: [non-fiction.bib, Thermo-Foam-Ref.bib]

---  

"
  zettel-title
  (format-time-string "%Y-%m-%d")
  )
  )

  ))

(global-set-key (kbd "<f5>") 'thermo-emacs-create-zettel)

(defun thermo-emacs-open-zettel-from-id (zettel-id)
  "Open the note associated with zettel-id; if there is none, displays an error message"
  (if (not (numberp zettel-id))
      (error "The provided zettel ID is not a number"))

  (let (matched-files target-file)

    (setq matched-files (directory-files deft-directory t (number-to-string zettel-id)))

    (if (not matched-files)
	(error "Could not find any files matching this ID"))

    (setq target-file (nth 0 matched-files))

    (find-file target-file)
))

(defun thermo-emacs-open-zettel-from-id-at-point ()
    "Open the zettel (in `deft-directory') associated with ID at point"
  (interactive)
  (thermo-emacs-open-zettel-from-id (thing-at-point 'number))
  )

(define-key markdown-mode-map (kbd "C-c f") 'thermo-emacs-open-zettel-from-id-at-point)

(defun thermo-emacs-copy-zettel-id ()
      "Puts the zettel ID of file in the current-buffer in the kill-ring"
      (interactive)
      (let (zettel-id)
        (string-match "\\([0-9]*\\)" (file-name-nondirectory (buffer-file-name)))
        (setq zettel-id (match-string 1 (file-name-nondirectory (buffer-file-name))))
        (if (> (length zettel-id) 0)
            (progn
              (kill-new zettel-id)
              (message "Zettel ID copied"))
            
          )
        )
      )

(define-key markdown-mode-map (kbd "C-c k") 'thermo-emacs-copy-zettel-id)


;; LATEX

(load "auctex.el" nil t t)
(load "preview.el" nil t t)
(load "texmathp.el" nil t t)

(setq TeX-autqo-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq TeX-PDF-mode t)

(require 'reftex)
(add-hook 'markdown-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(add-to-list 'load-path (expand-file-name "auctex-latexmk" dotfiles-dir))
(require 'auctex-latexmk)
(auctex-latexmk-setup)

(eval-after-load "latex"
'(define-key docTeX-mode-map (kbd "\C-c j") 'comment-indent-new-line))

(add-to-list 'TeX-expand-list
	     '("%(base-file-name)"
				(lambda ()
				  (concat "\"" (file-name-base) "\""))))
(add-to-list 'TeX-expand-list
	     '("%(pdf-file-name)"
				(lambda ()
				  (concat "\"" (file-name-base) ".pdf" "\""))))

(setq-default TeX-command-list
                (cons
                 '("LatexMk-Pnw" "latexmk %(-PDF)%S%(mode) %(file-line-error) %(base-file-name)" TeX-run-latexmk nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk")
                 TeX-command-list)
                LaTeX-clean-intermediate-suffixes
                (append LaTeX-clean-intermediate-suffixes
                        '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls")))
;; ORG-MODE

(require 'org)

; enable reftex mode in org-mode
(defun org-mode-reftex-setup ()
  (load-library "reftex") 
  (and (buffer-file-name)
  (file-exists-p (buffer-file-name))
  (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun thermo-emacs-open-dashboard()
  (interactive)
  (org-agenda "a" "d"))

(defun thermo-emacs-open-forecast()
  (interactive)
  (org-agenda "a" "a"))

(global-set-key (kbd "<f7>") 'thermo-emacs-open-dashboard)
(global-set-key (kbd "<f6>") 'thermo-emacs-open-forecast)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   ))

(setq te-main-task-file-name "tasks.org")

(setq te-main-task-file (expand-file-name te-main-task-file-name org-directory))

(defun te-open-main-task-file ()
  "Visit TE-MAIN-TASK-FILE in another window"
  (interactive)
  (find-file-other-window te-main-task-file))

(global-set-key (kbd "C-c o") 'te-open-main-task-file)

(defun te-org-agenda-check-and-refresh ()
  "Call the org-agenda functions to 'check' the current headline and refresh buffer"
  (interactive)
  (org-agenda-todo)
  (org-agenda-redo))

(require 'org-agenda)
(define-key org-agenda-mode-map (kbd "t") 'te-org-agenda-check-and-refresh)

(defun te-org-archive-done-tasks ()
  "Archive all DONE tasks in current file"
   (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

;; OS X SPECIFIC STUFF
(if (eq system-type 'darwin)
    (progn
      (exec-path-from-shell-initialize)
      (set-variable 'TeX-view-program-selection
                      '((output-pdf "Split-Skim")))
      (defadvice yes-or-no-p (around prevent-dialog activate)
	"Prevent yes-or-no-p from activating a dialog"
	(let ((use-dialog-box nil))
	  ad-do-it))
      (defadvice y-or-n-p (around prevent-dialog-yorn activate)
	"Prevent y-or-n-p from activating a dialog"
	(let ((use-dialog-box nil))
	  ad-do-it))
      (setenv "TMPDIR" "/tmp")
      ))



;; WINDOWS SPECIFIC STUFF
(if (eq system-type 'windows-nt)
    (progn
;      (set-face-font 'default "DejaVu Sans Mono")
      (set-face-font 'default "Consolas")
      (set-variable 'TeX-view-program-selection
		      '((output-pdf "Sumatra")))
      ; autohotkey
      (setq ahk-syntax-directory "C:/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax")
      (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
      (autoload 'ahk-mode "ahk-mode")
      ))

;; WEB-MODE

(add-to-list 'load-path (expand-file-name "web-mode/" dotfiles-dir))
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; REMOTE EDITING

(require 'tramp)
  (setq tramp-default-method "ssh")

(tramp-set-completion-function "ssh"
           '((tramp-parse-sconfig "/etc/ssh_config")
             (tramp-parse-sconfig "~/.ssh/config")))


; make the first running instance of emacs a server
(load "server")
(setq server-socket-dir (expand-file-name "server" dotfiles-dir))
(unless (server-running-p) (server-start))

;; auto-complete
(ac-config-default)

;; PYTHON
(elpy-enable)

(elpy-use-ipython)

(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 79)))

;; Polymode

(require 'polymode)

(defcustom  pm-inner/pweave
  (pm-hbtchunkmode "pweave"
                   :head-reg "^[ \t]*<<\\(.*\\)>>=\\|<%"
                   :tail-reg "^[ \t]*@ *\\( %def .*\\)?$\\|%>"
		   :mode 'python-mode)

  "Pweave chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/pweave
  (pm-polymode-one "pweave"
                   :hostmode 'pm-host/latex
                   :innermode 'pm-inner/pweave
)
  "Pweave configuration"
  :group 'polymodes
  :type 'object)

(define-polymode poly-pweave-mode pm-poly/pweave :lighter " Pweave")

(add-to-list 'auto-mode-alist '("\\.Pnw" . poly-pweave-mode))

;; helm

(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)

(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)
;;; init.el ends here
