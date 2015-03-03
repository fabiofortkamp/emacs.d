
(add-to-list 'load-path "~/.emacs.d/markdown-mode/")
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(markdown-enable-math)

(add-to-list 'load-path "~/.emacs.d/deft/")
(require 'deft)

(setq deft-extensions '("txt" "org" "taskpaper" "md"))
(setq deft-default-extension "md")
(setq deft-directory "~/Dropbox/notes")
(setq deft-text-mode 'markdown-mode)
(global-set-key [f8] 'deft)
(setq deft-use-filename-as-title t)
