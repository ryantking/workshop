{pkgs, ...}: {
  environment.systemPackages = with pkgs; [go_1_18 gotools gofumpt];
}
