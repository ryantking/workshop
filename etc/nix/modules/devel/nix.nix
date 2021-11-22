{ pkgs, lib, ... }:

{
  hm = {
    home.packages = with pkgs; [ cachix manix nixfmt nixpkgs-review nix-diff nix-index ];
  } // lib.optionalAttrs pkgs.stdenv.isLinux {
    systemd.user = {
      services.nix-index-update = {
        Unit = {
          Description = "tupdate the nix-index databes";
          After = [ "network.online.target" ];
        };

        Service = {
          Type = "oneshot";
          ExecStart = toString (pkgs.writeScript "nix-index-update" ''
            #!${pkgs.runtimeShell}
            tag=$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --tags --sort='v:refname' \
                https://github.com/Mic92/nix-index-database \
                | awk 'END {match($2, /([^/]+)$/, m); print m[0]}')
            curl -L "https://github.com/Mic92/nix-index-database/releases/download/$tag/files" -o $XDG_RUNTIME_DIR/files-$tag
            mv $XDG_RUNTIME_DIR/files-$tag $HOME/.cache/nix-index/files'');

          # This isn't urgent, just wait until no one else needs the disk
          IOSchedulingClass = "idle";
        };
      };

      timers.nix-index-update = {
        Unit.Description = "Update the nix-index database";
        Timer.OnCalendar = "Sunday 10:00";
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
