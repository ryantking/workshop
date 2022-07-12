{
  font,
  colors,
  padding,
  segment,
  shadow,
  ...
}: {
  bar = {
    height = 32;
    corner_radius = 0;
    border_width = 0;
    border_color = colors.bg;
    margin = 0;
    blur_radius = 30;
    position = "top";
    padding_left = 5;
    padding_right = 5;
    color = colors.bg;
    topmost = false;
    font_smoothing = false;
    y_offset = 0;
    shadow = true;
  };

  default = {
    drawing = true;
    lazy = false;
    updates = "when_shown";
    cache_scripts = true;
    "label.font" = ''"${font}:Semibold:13.0"'';
    "icon.font" = ''"${font}:Bold:14.0"'';
    "icon.color" = colors.fg;
    "label.color" = colors.fg;
    "icon.padding_left" = padding;
    "icon.padding_right" = padding;
    "label.padding_left" = padding;
    "label.padding_right" = padding;
    "background.padding_left" = padding;
    "background.padding_right" = padding;
    "background.color" = colors.none;
    "background.border_color" = colors.none;
    "background.border_width" = segment.borderWidth;
    "background.height" = segment.height;
    "background.corner_radius" = segment.cornerRadius;
    "background.drawing" = false;
    "icon.background.height" = segment.height;
    "icon.background.color" = colors.none;
    "icon.background.corner_radius" = segment.cornerRadius;
    "icon.background.drawing" = false;
    "label.background.color" = colors.none;
    "label.background.border_width" = segment.borderWidth;
    "label.background.border_color" = colors.none;
    "label.background.height" = segment.height;
    "label.background.corner_radius" = segment.cornerRadius;
    "label.background.drawing" = false;
    "popup.background.border_width" = 2;
    "popup.background.corner_radius" = segment.cornerRadius;
    "popup.background.border_color" = colors.bg;
    "popup.background.color" = colors.bgAlt;
    "popup.background.shadow.drawing" = true;
    "icon.shadow.drawing" = true;
    "label.shadow.drawing" = true;
    "alias.shadow.drawing" = true;
    "icon.shadow.color" = colors.bg;
    "label.shadow.color" = colors.bg;
    "alias.shadow.color" = colors.bg;
    "icon.shadow.distance" = shadow.distance;
    "label.shadow.distance" = shadow.distance;
    "alias.shadow.distance" = shadow.distance;
    "icon.shadow.angle" = shadow.angle;
    "label.shadow.angle" = shadow.angle;
    "alias.shadow.angle" = shadow.angle;
    "alias.color" = colors.fg;
  };
}
