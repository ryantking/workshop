;;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*Packages][Packages:1]]
(unpin! doom-themes doom-modeline)
(package! solaire-mode :disable t)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
;; Packages:1 ends here

;; [[file:config.org::*Packages][Packages:1]]
(unpin! lsp-mode lsp-Ui)
;; Packages:1 ends here

;; [[file:config.org::*Packages][Packages:1]]
(package! org-ref)
(package! org-roam-ui)
;; Packages:1 ends here
