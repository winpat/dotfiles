{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/lenovo/thinkpad/p14s/amd/gen2>
  ];

  networking = {
    hostName = "gem";
    hostId = "2a74eceb";
  };

  #boot.kernelPatches = [
  #  {
  #    name = "amdgpu-config";
  #    patch = null;
  #    extraConfig = ''
  #      DRM_AMD_DC_DCN1_0 y
  #    '';
  #  }
  #];

  boot.loader = {
    systemd-boot.enable = true;
  };

  # Enable thunderbolt docking station
  services.hardware.bolt.enable = true;

  boot.initrd.luks.devices = {
    luks1 = {
      device = "/dev/disk/by-uuid/90d7c9ae-2225-46f7-b787-4b3ae995e43d";
    };
  };


  networking.wireguard.interfaces = {
   wg0 = {
     ips = ["192.168.10.40/24"];
     privateKeyFile = "/home/patrick/.wireguard/private";
     peers = [
       {
         publicKey = "xDsBTXqmxgXDzGfRCxN6C5Qa80SyH/rCYbzo9zFLwj8=";
         allowedIPs = [
           "192.168.10.0/24"
           "192.168.1.0/24"
         ];
         endpoint = "vpn.winpat.ch:51820";
         persistentKeepalive = 25;
       }
     ];
   };
  };

  hardware.trackpoint = {
    device = "TPPS/2 Elan TrackPoint";
    sensitivity = 255;
    speed = 100;
  };

  services.xserver = {
    #dpi = 140;
    libinput = {
      enable = true;
      naturalScrolling = false;
      middleEmulation = true;
      tapping = true;
    };

  };

  # Auto load display layouts on hotplug
  #services.autorandr.enable = true;

  # Power savings
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
  services.tlp.enable = true;

  # Suspend to RAM by default
  boot.kernelParams = [ "mem_sleep_default=deep" ];
}
