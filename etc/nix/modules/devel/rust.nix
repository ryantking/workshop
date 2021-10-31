{ pkgs, ... }:

{
  hm.home = {
    packages = with pkgs; [ rustup ];
    sessionVariables = {
      RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
    };
  };
}
