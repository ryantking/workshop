{ pluginDir
, colors
, padding
, ...
}: [
  {
    name = "spotify.name";
    type = "clone";
    args = [ "label_template" ];
    settings = {
      "position" = "right";
      "click_script" = ''"sketchybar -m --set \$NAME popup.drawing=toggle"'';
      "popup.horizontal" = true;
      "popup.align" = "center";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
      "background.padding_left" = padding;
    };
  }
  {
    name = "spotify.back";
    args = [ "popup.spotify.name" ];
    events = [ "mouse.clicked" ];
    settings = {
      "icon" = "􀊎";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
    };
  }
  {
    name = "spotify.play_pause";
    args = [ "popup.spotify.name" ];
    events = [ "mouse.clicked" "spotify_change" ];
    settings = {
      "icon" = "􀊔";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
    };
  }
  {
    name = "spotify.next";
    args = [ "popup.spotify.name" ];
    events = [ "mouse.clicked" ];
    settings = {
      "icon" = "􀊐";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
      "label.padding_right" = 6;
    };
  }
  {
    name = "spotify.shuffle";
    args = [ "popup.spotify.name" ];
    events = [ "mouse.clicked" ];
    settings = {
      "icon" = "􀊝";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
      "icon.highlight_color" = colors.green;
    };
  }
  {
    name = "spotify.repeat";
    args = [ "popup.spotify.name" ];
    events = [ "mouse.clicked" ];
    settings = {
      "icon" = "􀊞";
      "script" = ''"${pluginDir}/spotify.sh"'';
      "updates" = true;
      "icon.highlight_color" = colors.green;
    };
  }
]
