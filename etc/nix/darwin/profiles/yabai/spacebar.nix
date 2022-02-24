{ config, pkgs, ... }:

let
  spacebarConfig = "${pkgs.writeScript "spacebarrc" ''#!/usr/bin/env sh

    spacebar -m config position             top
    spacebar -m config height               26
    spacebar -m config title                on
    spacebar -m config spaces               on
    spacebar -m config clock                on
    spacebar -m config power                on
    spacebar -m config padding_left         20
    spacebar -m config padding_right        20
    spacebar -m config spacing_left         25
    spacebar -m config spacing_right        15
    spacebar -m config text_font            "Helvetica Neue:Regular:12.0"
    spacebar -m config icon_font            "VictorMono Nerd Font:Regular:12.0"
    spacebar -m config background_color     0xff2e3440
    spacebar -m config foreground_color     0xffd8dee9
    spacebar -m config space_icon_color     0xff8fbcbb
    spacebar -m config power_icon_color     0xffebcbdb
    spacebar -m config battery_icon_color   0xffa3be8c
    spacebar -m config dnd_icon_color       0xffa3bE8c
    spacebar -m config clock_icon_color     0xff81a1C1
    spacebar -m config power_icon_strip      
    spacebar -m config space_icon_strip     I II III IV V VI VII VIII IX X
    spacebar -m config space_icon           
    spacebar -m config clock_icon           
    spacebar -m config dnd_icon             
    spacebar -m config clock_format         "%d/%m/%y %R"
    spacebar -m config right_shell          on
    spacebar -m config right_shell_command  whoami

    echo "spacebar configuration loaded.."
''}";

  inherit (config.home) cacheHome;
in
{
  services.spacebar = {
    enable = true;
    package = pkgs.spacebar;
    config = {
      position = "top";
      display = "all";
      height = 26;
      title = "on";
      spaces = "on";
      clock = "on";
      power = "on";
      padding_left = 20;
      padding_right = 20;
      spacing_left = 25;
      spacing_right = 15;
      text_font = ''"Helvetica Neue:Regular:12.0"'';
      icon_font = ''"VictorMono Nerd Font:Regular:12.0"'';
      background_color = "0xff2e3440";
      foreground_color = "0xffd8dee9";
      space_icon_color = "0xff8fbcbb";
      power_icon_color = "0xffebcbdb";
      battery_icon_color = "0xffa3be8c";
      dnd_icon_color = "0xffa3bE8c";
      clock_icon_color = "0xff81a1C1";
      power_icon_strip = " ";
      space_icon = "";
      space_icon_strip = "I II III IV V VI VII VIII IX X";
      spaces_for_all_displays = "on";
      display_separator = "on";
      display_separator_icon = "";
      clock_icon = "";
      dnd_icon = "";
      clock_format = ''"%d/%m/%y %R"'';
      right_shell = "off";
      # right_shell_icon           = "";
      # right_shell_command        = "whoami";
    };
  };

  launchd.user.agents.spacebar.serviceConfig = {
    #     ProgramArguments = [ "/usr/local/bin/spacebar" "-c" spacebarConfig ];
    #     EnvironmentVariables = {
    #       PATH = "${config.environment.systemPath}";
    #     };
    #     KeepAlive = true;
    #     RunAtLoad = true;
    StandardOutPath = "${cacheHome}/logs/spacebar.out.log";
    StandardErrorPath = "${cacheHome}/logs/spacebar.err.log";
  };
}
