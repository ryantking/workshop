{
  pluginDir,
  loadingIcon,
  font,
  colors,
  ...
}: [
  {
    name = "system.label";
    type = "clone";
    args = ["label_template"];
    settings = {
      label = "sys";
      position = "left";
      drawing = true;
      script = ''"${pluginDir}/window_title.sh"'';
    };
    events = ["front_app_switched"];
  }
  {
    name = "Control Center,Battery";
    type = "alias";
    args = ["left"];
    settings = {
      update_freq = 2;
      drawing = "$HAS_BATTERY";
      "icon.padding_left" = -5;
      "label.drawing" = false;
      "background.padding_left" = 0;
      "background.padding_right" = -3;
      click_script = ''"sketchybar -m --set \"\$NAME\" popup.drawing=toggle; sketchybar --trigger battery"'';
    };
  }
  {
    name = "battery.details";
    args = [''popup."Control Center,Battery"''];
    settings = {
      updates = "$HAS_BATTERY";
      script = ''"${pluginDir}/battery.sh"'';
      "label.padding_right" = 8;
    };
    events = ["battery"];
  }
  {
    name = "Control Center,WiFi";
    type = "alias";
    args = ["left"];
    settings = {
      update_freq = 3;
      "icon.drawing" = false;
      "label.drawing" = false;
      "background.padding_left" = -4;
      "background.padding_right" = -4;
      click_script = ''"sketchybar -m --set \"\$NAME\" popup.drawing=toggle; sketchybar --trigger wifi"'';
    };
  }
  {
    name = "wifi.details";
    args = [''popup."Control Center,WiFi"''];
    settings = {
      updates = true;
      script = ''"${pluginDir}/wifi.sh"'';
      "label.padding_right" = 5;
    };
    events = ["wifi"];
  }
  {
    name = "Control Center,Sound";
    type = "alias";
    args = ["left"];
    settings = {
      update_freq = 2;
      "icon.drawing" = false;
      "label.drawing" = false;
      "background.padding_left" = -4;
      "background.padding_right" = -4;
    };
  }
  {
    name = "system.mic";
    args = ["left"];
    settings = {
      "update_freq" = 100;
      "label.drawing" = false;
      script = ''"${pluginDir}/mic.sh"'';
      click_script = ''"${pluginDir}/mic_click.sh"'';
    };
  }
  {
    name = "system.caffeinate";
    args = ["left"];
    settings = {
      update_freq = 100;
      icon = loadingIcon;
      "label.drawing" = false;
      script = ''"${pluginDir}/caffeinate.sh"'';
    };
    events = ["mouse.clicked"];
  }
  {
    name = "system.yabai";
    args = ["left"];
    settings = {
      script = ''"${pluginDir}/yabai.sh"'';
      "icon.font" = ''"${font}:Bold:16.0"'';
      "label.drawing" = false;
      updates = true;
    };
    events = ["window_focus" "monocle" "mouse.clicked"];
  }
  {
    name = "system";
    type = "bracket";
    args = [
      "system.label"
      ''"Control Center,Battery"''
      ''"Control Center,WiFi"''
      ''"Control Center,Sound"''
      "system.mic"
      "system.caffeinate"
      "system.yabai"
    ];
    settings = {
      "background.drawing" = true;
    };
  }
]
