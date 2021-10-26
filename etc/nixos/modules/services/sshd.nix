{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.services.sshd;
in {
  options.modules.services.sshd.enable =
    mkEnableOption "SSH access to this machine";

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      passwordAuthentication = true;
    };

    user.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChmn6rG77RylvNxSDCuLX8IO6loqV7gNdsgqjq6ModHQshiVb1yG/zNvstS2fLWRdnKvQnhPj05xmQT0GNmL74+04hsQA2W2SJ6qECJ3QdvwztlB3d8QS2TXg9zmh1RoQRJ/aUKXEz0JV53V4SHcTgsUvApYcWXkNSbTxMl56BAOLr7xmHP1G6KBXD7rawJIrqpgtXmcxMaEelKGwp1Sj/byIZ+SrdiIfBGGR8R0aCNdyWi98YMFpTupzUjOmQjqTTG76DI6DVvwR1YTbYmoJ7BiWrmTr3ene2j7hfUmNGNpgm+psu1/yzg31yRLacfgYYwnZGQXo0oIFVcyJdLcVUfadtDxDfBcsWDCUMW2pEkTOUxWqjfns07RncOxhXyN6B/FgLdGK95KXygsrU907pYfwq/sxq1//1KQcl/yyCRNoEA/SHbwjB/wjI2BHgSjxnKtyYwP4HFL+JuLPfbtTigomPhFOE0fIUgasUqQrBQ0XTnuT2k4YM+vJRTgOHaYfRrgABzNDij5S/8RDlsZQFq+rKTdtT368Wp1Vu/m7sLAh1Ig6cBPenexgabgSaopNWA2fKA8GR+smNsFn5thMy5pYmthBRGi35F9pyaRZ/w158s9GQ5jgz8UFMu611RFrX+ZPq9CX3/ZV/Z+jAIKDvyDSn0O8Vxi9o6gTyG7vXzQ== (none)"
    ];
  };
}
