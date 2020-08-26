{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/lenovo/thinkpad/x1/6th-gen>
  ];

  networking = {
    hostName = "tron";
    hostId = "7a78ecea";
  };

  boot.loader = {
    systemd-boot.enable = true;
  };

  # Enable thunderbolt docking station
  services.hardware.bolt.enable = true;

  boot.initrd.luks.devices = {
    luks1 = {
      device = "/dev/disk/by-uuid/74eeb095-84c4-428a-b380-19124b55e0c3";
    };
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = ["192.168.10.20/24"];
      privateKeyFile = "/home/patrick/.wireguard/private";
      peers = [
        {
          publicKey = "WFsK1upCYpWmnJPqT+yFtgbSEJf6eh6sMVuYqfoFRDw=";
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
    dpi = 140;

    windowManager.openbox.enable = true;
    windowManager.i3.enable = true;
    libinput = {
      enable = true;
      naturalScrolling = false;
      middleEmulation = true;
      tapping = true;
    };

  };

  # Auto load display layouts on hotplug
  services.autorandr.enable = true;

  # Power savings
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
  services.tlp.enable = true;

  # Suspend to RAM by default
  boot.kernelParams = [ "mem_sleep_default=deep" ];

  environment.systemPackages = with pkgs; [
    # i3
    i3-layout-manager
    python37Packages.i3ipc

    # i3 Blocklets
    perl
    acpi  # battery
    i3blocks

    # openbox
    tint2
    obconf
    tint2
    gsimplecal
  ];
}
