{ config, lib, pkgs, ... }:

{
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    config = {
      external_bar = "all:32:0";
      layout = "bsp";

      mouse_follows_focus = "on";
      focus_follows_mouse = "off";
      mouse_modifier = "hyper";
      mouse_action1 = "move";
      mouse_action2 = "resize";

      window_placement = "second_child";
      window_topmost = "off";
      split_ratio = 0.5;
      auto_balance = "on";

      window_opacity = "on";
      window_shadow = "on";
      active_window_opacity = 1.0;
      normal_window_opacity = 0.98;

      window_border = "on";
      active_window_border_color = "0xFF${config.colorscheme.colors.base0C}";
      normal_window_border_color = "0x00${config.colorscheme.colors.base00}";
      insert_feedback_color = "0xFF${config.colorscheme.colors.base0B}";
      top_padding = 18;
      bottom_padding = 18;
      left_padding = 18;
      right_padding = 18;
      window_gap = 18;
    };

    spaces = [ "code" "web1" "term" "work" "chat" "misc" "media" "web2" "misc2" ];

    rules =
      let
        commonRules = {
          manage = false;
          sticky = true;
        };
      in
      [
        (commonRules // { app = "1Password"; })
        (commonRules // { app = "Alfred Preferences"; })
        (commonRules // { app = "Fantastical Helper"; })
        (commonRules // { app = "^System Preferences$"; })
        (commonRules // { app = "SentinelOne"; })
        {
          app = "zoom.us";
          opacity = "1.0";
        }
        {
          app = "Emacs";
          title = "doom-capture";
          manage = false;
          grid = "3:3:1:1:1:1";
          label = "[Emacs]: Float and center the doom capture window";
        }
        {
          app = "Emacs";
          title = ".*Minibuf.*";
          manage = false;
          border = false;
          label = "[Emacs]: Float minibuffer";
        }
      ];

    signals = [
      {
        event = "dock_did_restart";
        action = "sudo yabai --load-sa";
      }
      {
        event = "window_focused";
        action = "sketchybar --trigger window_focus";
      }
    ];
  };
}
