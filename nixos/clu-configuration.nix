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
