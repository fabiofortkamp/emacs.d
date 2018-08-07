(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("LatexMk-Pnw" "latexmk %(-PDF)%S%(mode) -bibtex %(file-line-error) %(base-file-name)" TeX-run-latexmk nil
      (plain-tex-mode latex-mode doctex-mode)
      :help "Run LatexMk")
     ("LatexMk" "latexmk %(-PDF)%S%(mode) %t" TeX-run-latexmk nil
      (plain-tex-mode latex-mode doctex-mode)
      :help "Run LatexMk")
     ("TeX" "%(PDF)%(tex) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %(base-file-name)" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls" TeX-run-command nil t :help "Create index for nomenclature package")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
     ("Sumatra"
      ("SumatraPDF.exe -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o"))
     ("Preview" "open %o -a Preview")
     ("PDF-XChange" "PDFXCview.exe %o")
     ("Split-Skim" "osascript ~/code/splitskim/splitskim.applescript %n %(pdf-file-name) %b \"Emacs + Skim\""))))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bibtex-autokey-prefix-string "bib:")
 '(bibtex-autokey-titleword-separator "-")
 '(bibtex-autokey-year-title-separator "-")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(cdlatex-math-symbol-prefix 64)
 '(comment-empty-lines t)
 '(custom-safe-themes
   (quote
    ("a6e5edd129bc48c9540ab6ed4a76a2e4e0da6359e981a3ec0bffbeb4416d4cc9" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "07dda9a3249f9ac909e7e0dc3c8876fd45898aa21646e093148dbd6ebb294f66" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(electric-indent-mode nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-timeout 10)
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(latex/cleanup-do-fill nil)
 '(latex/should-auto-fill-$ nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote none))
 '(org-agenda-custom-commands
   (quote
    (("d" "Dashboard" tags-todo "FLAGGED"
      ((org-agenda-todo-ignore-scheduled
	(quote future))))
     ("y" "All Available" alltodo ""
      ((org-agenda-todo-ignore-scheduled
	(quote future))))
     ("x" "Available at home" tags-todo "-errands-POLO"
      ((org-agenda-todo-ignore-scheduled
	(quote future)))))))
 '(org-agenda-files
   (quote
    ("~/latex-writing/paper-analytical-halbach/notes.org" "~/Dropbox/notes/20160331121300 Energy magnetic fields.org" "~/Dropbox/notes/tasks/tasks.org")))
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-ellipsis "  ")
 '(org-export-backends (quote (ascii html icalendar latex md org)))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-leading-stars t)
 '(org-log-repeat nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (yaml-mode solarized-theme polymode pandoc-mode neotree markdown-mode magit julia-mode jdee java-snippets helm-bibtex font-lock+ expand-region exec-path-from-shell ess elpy ein doom-themes deft column-marker cdlatex auctex ahk-mode)))
 '(reftex-cite-format
   (quote
    ((13 . "\\cite{%l}")
     (111 . "\\citeonline{%l}")
     (91 . "[@%l]")
     (64 . "@%l")
     (105 . "%l")
     (116 . "\\citet{%l}")
     (112 . "\\citep{%l}"))))
 '(reftex-default-bibliography
   (quote
    ("c:/Users/Fabio/thermo-ref/Thermo-Foam-Ref.bib" "c:/Users/Fabio/thermo-ref/non-fiction.bib")))
 '(reftex-label-alist (quote ((nil 101 nil "\\autoref{%s}" nil nil))))
 '(reftex-ref-style-default-list (quote ("Default" "Hyperref")))
 '(safe-local-variable-values
   (quote
    ((TeX-command-default . LatexMk)
     (TeX-command-default . LatexMk-Pnw)
     (engine . django))))
 '(show-paren-mode t)
 '(term-scroll-show-maximum-output t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks nil)
 '(web-mode-enable-engine-detection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "outline" :family "Consolas")))))
