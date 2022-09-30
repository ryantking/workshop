{
  self,
  config,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) colors;

  commonRules = {
    manage = false;
    sticky = true;
  };
in {
  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    config = {
      layout = "bsp";

      mouse_follows_focus = "on";
      focus_follows_mouse = "off";
      mouse_modifier = "fn";
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
      active_window_border_color = "0xFF${colors.base0C}";
      normal_window_border_color = "0x00${colors.base00}";
      insert_feedback_color = "0xFF${colors.base0B}";
      top_padding = 18;
      bottom_padding = 18;
      left_padding = 18;
      right_padding = 18;
      window_gap = 18;
    };

    spaces = ["code" "chat" "misc" "web" "" "misc2"];

    rules = [
      (commonRules // {app = "1Password";})
      (commonRules // {app = "Alfred Preferences";})
      (commonRules // {app = "^System Preferences$";})
      {
        app = "Hammerspoon";
        title = "";
        manage = true;
        sticky = true;
        layer = "above";
      }
      {
        app = "Hammerspoon Console";
        action = "yabai -m window \$(yabai -m query --windows | jq '.[] | select(.app==\"Hammerspoon\" and .name==\"Hammerspoon Console\") | .id') --minimize";
      }
      {
        app = "Emacs";
        title = ".*Minibuf.*";
        manage = false;
        border = false;
        label = "[Emacs]: Float minibuffer";
      }
      {
        app = "Emacs";
        title = "Emacs Everywhere ::*";
        manage = false;
        border = false;
        grid = "3:3:1:1:1:1";
        label = "[Emacs]: Float emacs-everywhere window";
      }
    ];

    signals = [
      {
        event = "dock_did_restart";
        action = "sudo yabai --load-sa";
      }
      {
        event = "window_destroyed";
        action = "hs -c \"hooks.windowDestroyed()\"";
      }
      {
        event = "application_activated";
        action = "hs -c \"hooks.applicationActivated(\`printenv YABAI_PROCESS_ID\`)\"";
      }
    ];
  };
}
