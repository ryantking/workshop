(import-macros {:let-g g-
                :def-keymap ki-
                :def-keymap-fn fki-} "zest.macros")

(local M {})

(fn M.autopairs []
  (let [{: setup : add_rule} (require "nvim-autopairs")
        rule (require "nvim-autopairs.rule")
        {: event} (require "cmp")]
    (setup
      {:disable_filetype ["TelescopePromt" "guihua"]
       :check_ts true
       :ts_config
       {:java false
        :go ["string"]
        :javascript ["template_string"]
        :lua ["string"]}})
    (add_rule (rule "'" "" ["fennel"]))
    (add_rule (rule "`" "" ["fennel"]))
    (event:on
      "confirm_done"
      (call "nvim-autopairs.completion.cmp" :on_confirm_done { :map_char {:tex "" }}))))

(fn M.matchup []
  (g- matchup_matchparen_offscreen {:method "popup"})
  ;(ki- "<C-k>" [n] "<C-u>MatchupWhereAmI?<cr>")
  )

(fn M.miniyank []
  (ki- "p" [n :remap] "<Plug>(miniyank-autoput)")
  (ki- "P" [n :remap] "<Plug>(miniyank-autoPut)"))

(fn M.move-pre [] (g- move_map_keys false))

(fn M.move []
  (map ["Up" "Down" "Left" "Right"]
    #(ki- (.. "<A-" $1 ">") [x :remap] (.. "<Plug>MoveBlock" $1)))
  (map ["Up" "Down"]
    #(ki- (.. "<A-" $1 ">") [n :remap] (.. "<Plug>MoveLine" $1)))
  (map ["Left" "Right"]
    #(ki- (.. "<A-" $1 ">") [n :remap] (.. "<Plug>MoveChar" $1))))

(fn M.wordmotion-pre [] (g- wordmotion_nomap 1))

(fn M.lightspeed []
  (map {:s "<Plug>Lightspeed_s" :S "<Plug>Lightspeed_S"
        :f "<Plug>Lightspeed_f" :F "<Plug>Lightspeed_F"
        :t "<Plug>Lightspeed_t" :T "<Plug>Lightspeed_T"}
    #(ki- $2 [n :silent :remap] $1))
  (map {:z "<Plug>Lightspeed_s" :Z "<Plug>Lightspeed_S"
        :f "<Plug>Lightspeed_f" :F "<Plug>Lightspeed_F"
        :t "<Plug>Lightspeed_t" :T "<Plug>Lightspeed_T"}
    #(ki- $2 [x :silent :remap] $1))
  (map {:z "<Plug>Lightspeed_s" :Z "<Plug>Lightspeed_S"
        :x "<Plug>Lightspeed_x" :X "<Plug>Lightspeed_X"
        :f "<Plug>Lightspeed_f" :F "<Plug>Lightspeed_F"
        :t "<Plug>Lightspeed_t" :T "<Plug>Lightspeed_T"}
    #(ki- $2 [o :silent :remap] $1)))

M
