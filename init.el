;; Thermo Emacs

;; this Org-mode implemenation was largely inspired by  Kieran Healy's Emacs Starter Kit
;;     https://github.com/kjhealy/emacs-starter-kit
;;
;; and follows the general template described in the Org Babel introduction
;;    http://orgmode.org/worg/org-contrib/babel/intro.html

;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; load up Org-mode and Org-babel
(require 'org-install)
(require 'ob-tangle)

;; load the main org mode file
(org-babel-load-file (expand-file-name "thermo-emacs.org"  dotfiles-dir))

;;; init.el ends here
