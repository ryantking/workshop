;;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*Rotate Windows][Rotate Windows:1]]
(package! rotate :pin "4e9ac3ff800880bd9b705794ef0f7c99d72900a6")
;; Rotate Windows:1 ends here

;; [[file:config.org::*Very Large Files][Very Large Files:1]]
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)
;; Very Large Files:1 ends here

;; [[file:config.org::*Auto Activating Snippets][Auto Activating Snippets:1]]
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "1699bec4d244a1f62af29fe4eb8b79b6d2fccf7d")
;; Auto Activating Snippets:1 ends here

;; [[file:config.org::*ETrace][ETrace:1]]
(package! etrace :recipe (:host github :repo "aspiers/etrace"))
;; ETrace:1 ends here

;; [[file:config.org::*String Inflection][String Inflection:1]]
(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")
;; String Inflection:1 ends here

;; [[file:config.org::*Info Colors][Info Colors:1]]
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")
;; Info Colors:1 ends here

;; [[file:config.org::*Prettier Page Breaks][Prettier Page Breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
;; Prettier Page Breaks:1 ends here

;; [[file:config.org::*Org][Org:1]]
(unpin! org-mode) ; there be bugs
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el"))
  :pin "b8012e759bd5bf5da802b0b41734a8fec218323c")
;; Org:1 ends here

;; [[file:config.org::*GUI][GUI:1]]
(unpin! doom-themes doom-modeline)
(package! solaire-mode :disable t)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
;; GUI:1 ends here

;; [[file:config.org::*LSP][LSP:1]]
;; (unpin! lsp-mode)
;; (unpin! lsp-ui)
;; LSP:1 ends here

;; [[file:config.org::*Packages][Packages:1]]
;; (package! org-appear)
;; (package! doct :recipe (:host github :repo "progfolio/doct"))
;; (package! org-padding :recipe (:host github :repo "TonCherAmi/org-padding"))
;; (package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
;; (package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))
;; (package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; (package! websocket)
;; Packages:1 ends here
