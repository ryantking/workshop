{ pluginDir
, font
, colors
, ...
}: [
  {
    name = "cpu.separator";
    args = [ "right" ];
    settings = {
      "icon.drawing" = false;
      "label.drawing" = false;
      "background.padding_left" = 0;
      "background.padding_right" = 0;
      "associated_space" = 1;
    };
  }
  {
    name = "cpu.topproc";
    args = [ "right" ];
    settings = {
      "label.font" = ''"${font}:Semibold:7"'';
      "label" = "CPU";
      "icon.drawing" = false;
      "width" = 0;
      "y_offset" = 6;
      "associated_space" = 1;
      "update_freq" = 5;
      "script" = ''"${pluginDir}/topproc.sh"'';
      "lazy" = true;
    };
  }
  {
    name = "cpu.percent";
    args = [ "right" ];
    settings = {
      "label.font" = ''"${font}:Heavy:12"'';
      "label" = "CPU";
      "y_offset" = -4;
      "width" = 40;
      "icon.drawing" = false;
      "update_freq" = 2;
      "associated_space" = 1;
      "lazy" = true;
    };
  }
  {
    name = "cpu.user";
    type = "graph";
    args = [ "right" "100" ];
    settings = {
      "graph.color" = colors.yellow;
      "update_freq" = 2;
      "width" = 0;
      "associated_space" = 1;
      "label.drawing" = false;
      "icon.drawing" = false;
      "background.height" = 23;
      "background.color" = colors.none;
      "background.border_color" = colors.none;
      "script" = ''"${pluginDir}/cpu.sh"'';
      "lazy" = true;
    };
  }
  {
    name = "cpu.sys";
    type = "graph";
    args = [ "right" "100" ];
    settings = {
      "associated_space" = 1;
      "graph.color" = colors.green;
      "label.drawing" = false;
      "icon.drawing" = false;
      "background.height" = 23;
      "background.color" = colors.none;
      "background.border_color" = colors.none;
    };
  }
  {
    name = "cpu.label";
    type = "clone";
    args = [ "label_template" ];
    settings = {
      "associated_space" = 1;
      "label" = "cpu";
      "position" = "rignt";
      "drawing" = true;
    };
  }
  {
    name = "cpu";
    type = "bracket";
    args = [ "cpu.separator" "cpu.topproc" "cpu.percent" "cpu.user" "cpu.sys" "cpu.label" ];
    settings = {
      "background.drawing" = true;
    };
  }
]
