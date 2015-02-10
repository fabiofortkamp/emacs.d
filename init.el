;; Thermo Emacs

; this Org-mode implemenation was largely inspired by  Kieran Healy's Emacs Starter Kit
; https://github.com/kjhealy/emacs-starter-kit

; set the Emacs directory programatically
(setq thermo-emacs-dir (file-name-directory (or (buffer-file-name) load-file-name)))

; load the basic babel file
(org-babel-load-file (expand-file-name "thermo-emacs.org" thermo-emacs-dir))
