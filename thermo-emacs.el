
;specifify the custom file
(setq custom-file (expand-file-name "emacs-custom.el" dotfiles-dir))
(load custom-file)

; add init directory to load path
(add-to-list 'load-path (expand-file-name "lisp" dotfiles-dir))

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(tool-bar-mode -1)

(global-linum-mode 1)

(global-visual-line-mode 1)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(put 'upcase-region 'disabled nil)

(org-babel-load-file (expand-file-name "thermo-emacs-keybindings.org" dotfiles-dir))

(setq package-enable-at-startup nil)
(package-initialize)

(org-babel-load-file (expand-file-name "thermo-emacs-notes.org" dotfiles-dir))

(org-babel-load-file (expand-file-name "thermo-emacs-latex.org" dotfiles-dir))

(org-babel-load-file (expand-file-name "thermo-emacs-org.org" dotfiles-dir))

(if (eq system-type 'darwin)
(org-babel-load-file (expand-file-name "thermo-emacs-osx.org" dotfiles-dir))
)

(if (eq system-type 'windows-nt)
      (org-babel-load-file (expand-file-name "thermo-emacs-windows.org" dotfiles-dir))
)
