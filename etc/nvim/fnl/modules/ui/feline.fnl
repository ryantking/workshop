(local colors (require "colors"))

(local vi-mode-colors
  {:NORMAL colors.primary
   :INSERT colors.secondary
   :VISUAL colors.tertiary
   :LINES colors.tertiary
   :BLOCK colors.secondary
   :OP colors.primary
   :REPLACE colors.quaternary
   :V-REPLACE colors.quaternary
   :ENTER colors.quinary
   :MORE colors.quinary
   :SELECT colors.senary
   :COMMAND colors.septary
   :SHELL colors.primary
   :TERM colors.primary
   :NONE colors.alert})

(local vi-mode-text
  {:n :NORMAL
   :i :INSERT
   :v :VISUAL
   "" :V-BLOCK
   :V :V-LINE
   :c :COMMAND
   :no :UNKNOWN
   :s :UNKNOWN
   :S :UNKNOWN
   :ic :UNKNOWN
   :R :REPLACE
   :Rv :UNKNOWN
   :cv :UNKWON
   :ce :UNKNOWN
   :r :REPLACE
   :rm :UNKNOWN
   :t :INSERT})

(fn make-component [provider opts]
  {:provider provider
   :left_sep (or (?. opts :sep) " ")
   :icon (?. opts :icon)
   :hl
   (or
     (?. opts :hl )
     {:fg (?. opts :fg)
      :bg (?. opts :bg)
      :style (or (?. opts :style) "bold")})})

(fn vi-mode-hl []
  (let [vi-mode (require "feline.providers.vi_mode")]
    {:name (vi-mode.get_mode_highlight_name)
     :fg colors.bg0
     :bg (vi-mode.get_mode_color)
     :style "bold"}))

(fn vi-mode-provider []
  (.. " " (. vi-mode-text (nvim.fn.mode)) " "))

(fn fname-provider []
  (fn split-fname [fname]
    (-> fname (trim "/") (split "/") (reverse)))

  (let [cur-fname (nvim.fn.expand "%s")
        cur-segs (split-fname cur-fname)]
    (fn needed-fname-segments []
      (fn needed-segs-for-file [fname]
        (-> fname
            (split-fname)
            (map #(-> cur-segs (. $2) (= $1)))
            (find false)
            (or 1)))

      (-> (nvim.fn.getbufinfo)
          (filter #(-> $1 (. :listed) (= 1)))
          (map #(. $1 :name))
          (reject #(string.match $1 (.. cur-fname "$")))
          (map needed-segs-for-file)
          (reduce #(math.max $1 $2) 1)))

    (let [n (needed-fname-segments cur-segs)]
      (-> cur-segs
          (first n)
          (reduce #(.. $2 "/" $1) "")
          (trim "/")))))

(fn file-os-provider []
  (-> vim.bo.fileformat
      (string.lower)
      (#(. {:unix " " :mac " "} $1))
      (#(if $1 $1 " "))))

(fn position-provider []
  (.. " " (call "feline.providers.cursor" :position) " "))

(local components
  {:active
   [[(make-component vi-mode-provider {:hl vi-mode-hl :sep ""})
     (make-component fname-provider {:fg colors.fg3})]
    []
    [(make-component "git_diff_added" {:fg colors.secondary})
     (make-component "git_diff_changed" {:fg colors.quaternary})
     (make-component "git_diff_removed" {:fg colors.alert})
     (make-component file-os-provider {:fg colors.tertiary})
     (make-component "git_branch" {:icon " " :fg colors.tertiary})
     (make-component "scroll_bar" {:fg colors.primary})
     (make-component "line_percentage")
     (make-component position-provider {:hl vi-mode-hl})]]
   :inactive
   [[(make-component fname-provider {:fg colors.fg2})]
    []
    [(make-component file-os-provider {:fg colors.fg2})]]})

(call "feline" :setup
  {:components components
   :colors {:fg colors.fg3 :bg colors.bg1}
   :default_hl {:fg colors.fg2 :bg colors.bg2}
   :vi_mode_colors vi-mode-colors})
