{ config, pkgs, ... }:

{
  networking = {
    hostName = "clu";
    hostId = "1428baea";
  };

  # Wasted to much time getting GRUB to work.
  boot.loader = {
    systemd-boot.enable = false;
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      devices = [ "nodev" ];
      efiSupport = true;
      useOSProber = true;
    };
  };

  services.xserver = {
    dpi = 110;
  };

  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";

    # make sure tailscale is running before trying to connect to tailscale
    after = [ "network-pre.target" "tailscale.service" ];
    wants = [ "network-pre.target" "tailscale.service" ];
    wantedBy = [ "multi-user.target" ];

    # set this service as a oneshot job
    serviceConfig.Type = "oneshot";

    # have the job run this shell script
    script = with pkgs; ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${tailscale}/bin/tailscale status -json | ${jq}/bin/jq -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${tailscale}/bin/tailscale up -authkey tskey-kESbvK2CNTRL-vUvgv4YdR9PqiaentC7fBL
    '';
  };


  # Use proprietary graphic card driver for CUDA
  #services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.nvidia.optimus_prime.nvidiaBusId = "PCI:11:0:0";
  # hardware.opengl.driSupport32Bit = true;
  # virtualisation.docker.enableNvidia = true;
  # systemd.services.nvidia-control-devices = {
  #   wantedBy = [ "multi-user.target" ];
  #   serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  # };

  # environment.systemPackages = with pkgs; [
  #   # Machine Learning
  #   cudatoolkit
  # ];
}
