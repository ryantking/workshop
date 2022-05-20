{ pkgs, ... }: {
  hardware = {
    bluetooth = {
      enable = true;
      settings = { General.Enable = "Source,Sink,Media,Socket"; };
    };

    pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  };
}
