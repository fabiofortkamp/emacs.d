
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

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

(eval-after-load "latex"
'(define-key docTeX-mode-map (kbd "\C-c j") 'comment-indent-new-line))
