{font, ...}: let
  mkMenuItem = {
    name,
    icon,
    label,
    script,
  }: {
    name = "apple.${name}";
    args = ["popup.apple.logo"];
    settings = {
      inherit icon label;
      click_script = ''"${script}; sketchybar -m --set apple.logo popup.drawing=off"'';
    };
  };
in
  builtins.concatLists [
    [
      {
        name = "apple.logo";
        args = ["left"];
        settings = {
          click_script = ''"sketchybar -m --set \$NAME popup.drawing=toggle"'';
          icon = "􀣺";
          "icon.font" = ''"${font}:Black:16.0"'';
          "label.drawing" = false;
          "background.padding_right" = 0;
        };
      }
    ]
    (map mkMenuItem [
      {
        name = "apple.preferences";
        icon = "􀺽";
        label = "Preferences";
        script = "open -a 'System Preferences";
      }
      {
        name = "apple.activity";
        icon = "􀒓";
        label = "Activity";
        script = "open -a 'Activity Monitor'";
      }
      {
        name = "apple.lock";
        icon = "􀒳";
        label = ''"Lock Screen"'';
        script = "pmset displaysleepnow";
      }
    ])
  ]
