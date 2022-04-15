{ barConfig }:

with barConfig;

[
  {
    name = "mailIndicator";
    args = [ "right" ];
    settings = {
      update_freq = 30;
      script = ''"${pluginDir}/mailIndicator.sh"'';
      icon = "􀍜";
      label = "!";
      "icon.font" = ''"${font}:Bold:16.0"'';
      "label.padding_right" = 8;
      "background.padding_right" = 0;
    };
  }
  {
    name = "MeetingBar";
    type = "alias";
    args = [ "right" ];
    settings = {
      update_freq = 60;
      "background.padding_right" = -8;
      "background.padding_left" = -6;
    };
  }
  {
    name = "calendar.time";
    args = [ "right" ];
    settings = {
      update_freq = 2;
      "icon.drawing" = false;
      script = ''"${pluginDir}/time.sh"'';
    };
  }
  {
    name = "calendar.date";
    args = [ "right" ];
    settings = {
      update_freq = 60;
      position = "right";
      label = "cal";
      drawing = true;
      "background.padding_right" = 0;
      script = ''"${pluginDir}/date.sh"'';
    };
  }
  {
    name = "calendar";
    type = "bracket";
    args = [ "mailIndicator" "MeetingBar" "calendar.time" "calendar.date" ];
    settings = {
      "background.drawing" = true;
    };
  }
]
