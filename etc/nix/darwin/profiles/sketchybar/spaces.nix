{ lib
, pluginDir
, font
, colors
, ...
}:
let
  spaceColors = with colors; [ none green yellow orange red magenta blue ];

  mkSpaceIcon = displayNdx: n: space: {
    inherit (space) name;
    type = "clone";
    args = [ "space_template" ];
    settings = {
      inherit (space) icon;
      associated_space = ((displayNdx - 1) * 6) + n;
      "icon.highlight_color" = builtins.elemAt spaceColors n;
      "icon.background.color" = builtins.elemAt spaceColors n;
      drawing = true;
    };
  };

  mkSpaceLabel = n: {
    name = "spaces_${toString n}.label";
    type = "clone";
    args = [ "label_template" ];
    settings = {
      label = "spc";
      "label.width" = 48;
      "label.align" = "center";
      associated_display = n;
      position = "center";
      drawing = true;
    };
  };

  mkSpaceBracket = n: spaces: {
    name = "spaces_${toString n}";
    type = "bracket";
    args = [ "spaces_${toString n}.label" ] ++ (map (builtins.getAttr "name") spaces);
    settings = {
      "background.drawing" = true;
    };
  };

  mkSpacesForDisplay = displayNdx: spaces:
    builtins.concatLists [
      [
        {
          name = "space_template";
          type = "set";
          settings = {
            associated_display = displayNdx;
          };
        }
        (mkSpaceLabel displayNdx)
      ]
      (lib.imap1 (mkSpaceIcon displayNdx) spaces)
      [ (mkSpaceBracket displayNdx spaces) ]
    ];

  mkSpaces = displays:
    builtins.concatLists [
      [
        {
          name = "space_template";
          type = "space";
          args = [ "center" ];
          settings = {
            "icon.highlight_color" = colors.green;
            "label.drawing" = false;
            drawing = false;
            updates = true;
            script = ''"${pluginDir}/space.sh"'';
            "label.font" = ''"${font}:Black:13.0"'';
            "icon.font" = ''"${font}:Bold:17.0"'';
            "icon.padding_right" = 6;
            "icon.padding_center" = 3;
            "background.padding_center" = 2;
            "background.padding_right" = 2;
            "icon.background.height" = 2;
            "icon.background.color" = colors.fg;
            "icon.background.y_offset" = -12;
            click_script = ''"yabai -m space --focus \$SID"'';
          };
        }
      ]
      (builtins.concatLists (lib.imap1 mkSpacesForDisplay displays))
    ];

  mkSpace = name: icon: { inherit name icon; };
in
mkSpaces [
  [
    (mkSpace "code" "􀤙")
    (mkSpace "web1" "􀆪")
    (mkSpace "term" "􀩼")
    (mkSpace "work" "􀷾")
    (mkSpace "chat" "􀕻")
    (mkSpace "misc1" "􀍠")
  ]
  [
    (mkSpace "music" "􀑈")
    (mkSpace "web2" "􀆪")
    (mkSpace "misc2" "􀍠")
  ]
]
