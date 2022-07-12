{
  pluginDir,
  loadingIcon,
  font,
  colors,
  ...
}: [
  {
    name = "github.label";
    type = "clone";
    args = ["label_template"];
    settings = {
      associated_space = 1;
      label = "git";
      position = "left";
      drawing = true;
    };
  }
  {
    name = "github.bell";
    args = ["left"];
    settings = {
      associated_space = 1;
      update_freq = 180;
      "icon.font" = "${font}:Bold:15.0";
      icon = "􀋙";
      label = loadingIcon;
      script = ''"${pluginDir}/gitNotifications.sh"'';
      click_script = ''"sketchybar --set \$NAME popup.drawing=toggle"'';
    };
  }
  {
    name = "github.commits";
    args = ["left"];
    events = ["git_push"];
    settings = {
      associated_space = 1;
      "icon.color" = colors.white;
      "icon.highlight_color" = colors.green;
      "icon.font" = "${font}:Bold:16.0";
      icon = "􀂓";
      update_freq = 1000;
      label = loadingIcon;
      click_script = ''"open https://github.com"'';
      script = ''"${pluginDir}/githubIndicator.sh"'';
    };
  }
  {
    name = "packages";
    args = ["left"];
    events = ["brew upgrade" "mouse.clicked"];
    settings = {
      "associated_space" = 1;
      update_freq = 18000;
      script = ''"${pluginDir}/package_monitor.sh"'';
      label = loadingIcon;
      icon = "􀐛";
    };
  }
  {
    name = "github";
    type = "bracket";
    args = ["github.label" "github.bell" "github.commits" "packages"];
    settings = {
      "background.drawing" = true;
    };
  }
]
