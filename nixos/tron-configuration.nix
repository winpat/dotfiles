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
      device = "/dev/disk/by-uuid/4c4a30bb-d6e5-4562-b29e-6ce5cb6acbb3";
    };
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
      ${tailscale}/bin/tailscale up -authkey tskey-kNX5s55CNTRL-NVcpsjyZs8XzAB8CAV8rW7
    '';
  };


  hardware.trackpoint = {
    device = "TPPS/2 Elan TrackPoint";
    sensitivity = 255;
    speed = 100;
  };

  services.xserver = {
    dpi = 140;
    libinput = {
      enable = true;
      touchpad = {
        middleEmulation = true;
        naturalScrolling = false;
        tapping = true;
      };
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
}
