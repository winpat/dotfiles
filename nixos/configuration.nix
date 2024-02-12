# configuration.nix of Patrick Winter <patrickwinter@posteo.ch>

{ config, pkgs, ... }:


{
  imports = [
    /etc/nixos/work.nix  # Work sensitive settings which are not published
    /etc/nixos/host-configuration.nix
    /etc/nixos/hardware-configuration.nix
  ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
    experimental-features = nix-command flakes
  '';
  };

  # Let me install packages from the unstable channel
  nixpkgs.config = {
    packageOverrides = pkgs: {
      # Allow to specify master/unstable packages declaritvely.
      # https://stackoverflow.com/questions/48831392/how-to-add-nixos-unstable-channel-declaratively-in-configuration-nix
      unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {
        config = config.nixpkgs.config;
      };
      master = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {
        config = config.nixpkgs.config;
      };
    };
  };

  # Required to install spotify, unrar, discord, ...
  nixpkgs.config.allowUnfree = true;

  # Give me the newest features!
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-amd" "kvm-intel" ];
    plymouth.enable = true;
    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      allowedTCPPorts = [ 22 ];
      checkReversePath = "loose";
    };
    hosts = {
      "100.118.247.61" = [ "mcp" ];
    };
  };

  services.tailscale.enable = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    keyMap = "de_CH-latin1";
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  # List of font packages that are exposed to applications
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [ hack-font font-awesome ];
  };

  # Required so libreoffice finds the hunspell dicts
  # https://github.com/NixOS/nixpkgs/pull/80329
  # https://github.com/NixOS/nixpkgs/pull/80353
  environment.pathsToLink = [ "/share/hunspell" ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [

    # Base
    vim
    tmux
    git
    file
    gnumake
    man-pages
    jq
    yq
    htop
    strace
    bind
    fasd
    direnv
    fzf
    bat
    ripgrep
    fd
    tree
    difftastic

    # Networking
    nmap
    inetutils # telnet
    netcat

    # HTTP
    wget
    curl
    httpie

    # Emacs
    emacs29

    # Misc
    sqlite

    # Spell checking
    hunspell
    hunspellDicts.de-ch
    hunspellDicts.en-us-large

    # Desktop
    alacritty
    dmenu
    xcape
    xorg.xev
    xorg.xmodmap
    xclip
    xsel
    xdotool
    wmctrl
    feh
    arandr
    libnotify
    dunst
    flameshot  # scrot on steroids
    gparted
    pavucontrol
    acpi

    # openbox
    tint2
    obconf

    # Diagrams
    graphviz
    plantuml

    # Writing
    texlive.combined.scheme-full
    libreoffice
    pandoc
    pdftk
    poppler_utils
    pdfgrep

    # Remote file systems
    sshfs-fuse
    cifs-utils

    # For when I am traveling
    mosh

    # Misc Desktop Applications
    dmidecode
    pciutils
    binutils
    usbutils
    pmutils

    # Lock screen
    i3lock-color

    # GTK theme
    arc-theme
    arc-icon-theme

    # Compression
    zip
    unzip
    unrar

    # Communication
    weechat
    slack
    unstable.discord

    # Databases
    pgcli
    pspg

    # Browser
    firefox
    chromium

    # Tray applications
    pasystray
    networkmanagerapplet

    # Network
    networkmanager
    networkmanager-openvpn
    openvpn
    tailscale

    # Fix missing icon for networkmanageapplet
    # https://github.com/NixOS/nixpkgs/issues/32730
    hicolor-icon-theme

    # Virtualization and containers
    virt-viewer
    virt-manager
    vagrant
    docker
    docker-compose

    # Media
    pcmanfm
    pcmanfm-qt
    spotify
    mpv
    vlc
    playerctl
    gimp
    gcolor3
    noisetorch
    imagemagick
    ffmpeg
    zathura
    xournalpp
    xarchiver

    # Security
    gnupg
    pass
    bitwarden
    bitwarden-cli
    pwgen
    yubikey-personalization
    pinentry-gnome

    # File synchronization
    rsync
    unison

    # Clipboard manager
    copyq

    # Ergodox Ez
    wally-cli

    # C
    gdb
    gcc
    zig
    cmake

    # Python 3
    python3
    python3Packages.ipython
    python3Packages.ipdb

    # Data Science
    visidata
    python3Packages.numpy
    python3Packages.pandas

    # JavaScript
    nodejs

    # Development
    pre-commit
    cookiecutter

    # Nix awesomeness
    cachix
    nixfmt

    # Electronics
    picocom

    # GCP
    (google-cloud-sdk.withExtraComponents [google-cloud-sdk.components.gke-gcloud-auth-plugin])
    kubectx
    kubectl
    k9s

    # Misc
    asciiquarium
    cbonsai
  ];

  services.syncthing = {
    enable = true;
    user = "patrick";
    dataDir = "/home/patrick";
    overrideFolders = false;
    overrideDevices = true;
    settings.devices = {
      mcp = {
        addresses = [ "tcp://100.118.247.61:22000" ];
        id = "5XOROW7-FWKKSXP-YNUDYZK-XVZ4LMK-2VY474C-DGG33XP-4BSXPPT-VYOT2A2";
      };
    };
  };

  services.udev.packages = with pkgs; [
    yubikey-personalization
    libu2f-host
  ];

  hardware.keyboard.zsa.enable = true;

  users.users.patrick = {
    isNormalUser = true;
    home = "/home/patrick";
    description = "Patrick Winter";
    extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "systemd-journal" "dialout"];
  };

  # Use a dedicated user for pair programming
  users.users.pair = {
    isNormalUser = true;
    description = "User for tmux-based pair programming";
    openssh.authorizedKeys.keys = [
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCWlePj9VDxv236PwNMB+yEHPe6S3fA0f8OLPDwapwCb35YfprCBMrKWJz8O62RynXi0Cb3087KB8lePEKcrvmsuth+z071WdjmRsU7x36lonfJ3TFIi6vi+/vC9Mx2fHvCycKQqfY/rvL6bmvufRZC2l5BIh1s8ZpIaKdQJc4mREEVaYf8ybyEr/D3hu36NWn9rNGEiIRINNOrgmFRFJvtRlb/YuCa/5ZY5FOL968CAgqphER5CpJ0LzBvCKrBKegWdqUfgjtkkzmcEVRZo2GGH7Cu9tCHYMT0DRSTn0RHNaqj4tp5Hz2zDWVVLptJQYis5M/8HzWIVO7KJPFhN/SZ me"
    ];
  };

  programs.bash.enableCompletion = true;

  security.sudo.wheelNeedsPassword = false;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
    };
  };

  services.nfs.server.enable = true;

  programs.ssh = {
    startAgent = true;
    agentTimeout = "168h";
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };

  # For when I am on the run :-)
  programs.mosh.enable = true;

  fileSystems."/net/media" = {
    device = "//192.168.0.200/media";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/home/patrick/.samba/personal"];
  };

  # Enable smart card reader
  services.pcscd.enable = true;

  # Start an emacs user service
  services.emacs = {
    enable = true;
    package = pkgs.emacs29;
    defaultEditor = true;
  };

  virtualisation = {
    libvirtd.enable = true;
    docker.enable = true;
  };

  # Use composite manager for extra fanciness
  services.compton.enable = true;
  services.unclutter.enable = true;

  services.xserver = {
    enable = true;
    layout = "ch";
    displayManager.lightdm.enable = true;
    windowManager.openbox.enable = true;

    # Lock the screen after 60 seconds of inactivity
    xautolock = {
      enable = true;
      locker = "${pkgs.i3lock-color}/bin/i3lock-color -c 000000 -k --date-color ffffff --time-color ffffff --date-str='%d/%m/%Y'";
      time = 3;
    };
  };

  # Gotta get that melatonine
  services.redshift.enable = true;

  location = {
    latitude = 47.519093;
    longitude = 8.017178;
  };

  # Discover network printers
  services.avahi.enable = true;

  # NixOS allows either a lightweight build (default) or full that includes
  # bluetooth support.
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  services.blueman.enable = true;

  # Enable bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  programs.noisetorch.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
