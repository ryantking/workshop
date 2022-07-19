{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) colors;

  lock = pkgs.writeShellScript "lock" ''
    ${pkgs.swaylock-effects}/bin/swaylock -f \
        --screenshots \
        --clock \
        --indicator \
        --indicator-radius 100 \
        --indicator-thickness 7 \
        --effect-blur 7x5 \
        --effect-vignette 0.5:0.5 \
        --ring-color ${colors.base02} \
        --key-hl-color ${colors.base0B} \
        --line-color ${colors.base00} \
        --inside-color ${colors.base06} \
        --separator-color ${colors.base01} \
        --grace 2 \
        --fade-in 0.2'';

  idleManager = let
    wlopm = "${pkgs.wlopm}/bin/wlopm";
  in
    pkgs.writeShellScript "idle-manager" ''
      ${pkgs.swayidle}/bin/swayidle -w \
          timeout 5n '${lock}' \
          timeout 10 '${wlopm} --off \*' \
          resume '${wlopm} --on \*' \
          before-sleep '${lock}'
    '';
in {
  environment.systemPackages = with pkgs; [
    go
    river
    wdisplays
    wayland-utils

    kanshi
    swaylock-effects
    swayidle
    swaybg
    mako
    wofi
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  security.pam.services.swaylock = {};

  xdg.portal.wlr.enable = true;

  programs.xwayland.enable = true;

  home.configFile = {
    "river/init".source = pkgs.writeShellScript "init" ''
      source ${config.home.configHome}/colorrc

      KANSHI_PATH=${pkgs.kanshi}/bin/kanshi
      SWAYLOCK_PATH=${pkgs.swaylock}/bin/swaylock
      SWAYIDLE_PATH=${pkgs.swayidle}/bin/swayidle
      SWAYBG_PATH=${pkgs.swaybg}/bin/swaybg
      MAKO_PATH=${pkgs.mako}/bin/make
      WOFI_PATH=${pkgs.wofi}/bin/wofi

      # pgrep kanshi && pkill -9 kanshi || ${pkgs.kanshi}/bin/kanshi &
      # pgrep idle && pkill -9 idle || ${idleManager} &

      ${pkgs.go}/bin/go run ${config.workshop.home}/bin

      riverctl default-layout rivertile
      pgrep rivertile && pkill -9 rivertile || rivertile -view-padding 12 -outer-padding 12'';

    "kanshi/config".text = ''        profile {
      output "DELL U3219Q" mode 3840x2160 position 0,0
      output "DELL U2718Q" mode 3840x2160 position 3840,0 transform 90
      }'';
  };

  # TODO: This should be a separate profile module so wayland WMs can enable it.
  services.greetd = {
    enable = true;
    settings.default_session.command = ''
      ${pkgs.greetd.tuigreet}/bin/tuigreet \
      --time \
      --asterisks \
      --greeting "Access is restricted to authorized users only." \
      --remember \
      --cmd ${pkgs.writeShellScript "tuigreet-cmd.sh" ''
        export XDG_SESSION_TYPE=wayland
        export XDG_SESSION_DESKTOP=sway
        export XDG_CURRENT_DESKTOP=sway
        export MOZ_ENABLE_WAYLAND=1
        export CLUTTER_BACKEND=wayland
        export QT_QPA_PLATFORM=wayland-eglr
        export ECORE_EVAS_ENGINE=wayland-egl
        export ELM_ENGINE=wayland_egl
        export SDL_VIDEODRIVER=wayland
        export _JAVA_AWT_WM_NONREPARENTING=1
        export NO_AT_BRIDGE=1
        export WLR_NO_HARDWARE_CURSORS=1
        exec ${pkgs.dbus}/bin/dbus-run-session -- river
      ''};
    '';
  };
}
