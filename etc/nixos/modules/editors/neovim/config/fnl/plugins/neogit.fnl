(module plugins.neogit {autoload {: neogit : diffview}})

(diffview.setup {})

(neogit.setup {:signs {:section ["▶" "▼"]
                       :item ["▶" "▼"]
                       :hunk ["" ""]}
               :integrations {:diffview true}})
