# configuration.nix of Patrick Winter <patrickwinter@posteo.ch>

{ config, pkgs, ... }:


{
  imports = [
    /etc/nixos/work.nix  # Work sensitive settings which are not published
    /etc/nixos/host-configuration.nix
    /etc/nixos/hardware-configuration.nix
  ];

  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
    experimental-features = nix-command flakes
  '';
  };

  # Let me install packages from the unstable channel
  nixpkgs.config = {
    packageOverrides = pkgs: {
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
      "127.0.0.1" = [ "postgres" "redis" ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "de_CH-latin1";
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

  environment.systemPackages = with pkgs; [
    # Base
    vim
    tmux
    file
    gnumake
    just
    man-pages
    jq
    htop
    strace
    bind
    ripgrep
    fd
    tree
    difftastic

    # Shell
    fzf
    fasd
    complete-alias

    # VCS
    git
    git-absorb
    gh
    glab

    # Networking
    nmap
    inetutils # telnet
    netcat

    # HTTP
    wget
    curl

    # Misc
    sqlite
    rlwrap

    # Spell checking
    hunspell
    hunspellDicts.de-ch
    hunspellDicts.en-us-large

    # Desktop
    ghostty
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
    dunst
    flameshot
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
    poppler-utils
    pdfgrep

    # Remote file systems
    sshfs-fuse
    cifs-utils

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
    xarchiver

    # Communication
    slack
    discord

    # Databases
    postgresql
    pgcli
    pspg

    # Browser
    firefox

    # Tray applications
    pasystray
    networkmanagerapplet

    # Network
    networkmanager
    networkmanager-openvpn
    openvpn

    # Fix missing icon for networkmanageapplet
    # https://github.com/NixOS/nixpkgs/issues/32730
    hicolor-icon-theme

    # Media
    pcmanfm-qt
    spotify
    mpv
    vlc
    playerctl
    gimp
    gcolor3
    imagemagick
    ffmpeg
    zathura
    xournalpp

    # Security
    pwgen
    gnupg
    pass
    bitwarden-desktop

    # File synchronization
    rsync

    # Clipboard manager
    copyq

    # Ergodox Ez
    wally-cli

    # C
    gdb
    gcc
    ccls
    cmake

    # Zig
    zig
    zls

    # Lisp
    clojure
    janet

    # Python 3
    python313
    python312
    python311
    python310
    pyright
    ruff
    mypy
    uv
    python3Packages.ipython
    python3Packages.ipdb

    # Data Science
    visidata
    python3Packages.numpy
    python3Packages.pandas

    # Development
    pre-commit

    # GCP
    (google-cloud-sdk.withExtraComponents ([google-cloud-sdk.components.gke-gcloud-auth-plugin]))
    kubectx
    kubectl
    k9s
    kubernetes-helm

    # Misc
    asciiquarium
    cbonsai

    # AI
    master.claude-code
  ];

  users.users.patrick = {
    isNormalUser = true;
    home = "/home/patrick";
    description = "Patrick Winter";
    extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "systemd-journal" "dialout"];
  };

  services.udisks2.enable = true;

  programs.nix-ld.enable = true;

  programs.direnv.enable = true;

  programs.bash.completion.enable = true;

  security.sudo.wheelNeedsPassword = false;

  services.openssh.enable = true;

  programs.ssh.startAgent = true;

  programs.mosh.enable = true;

  services.tailscale.enable = true;

  programs.gnupg.agent.enable = true;

  services.pcscd.enable = true;

  virtualisation.docker.enable = true;

  hardware.keyboard.zsa.enable = true;

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

  fileSystems."/net/media" = {
    device = "//192.168.0.200/media";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/home/patrick/.samba/personal"];
  };

  fileSystems."/net/guitar" = {
    device = "//192.168.0.200/guitar";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/home/patrick/.samba/personal"];
  };

  # Start an emacs user service
  services.emacs = {
    enable = true;
    package = pkgs.emacs30;
    defaultEditor = true;
  };

  services.picom.enable = true;
  services.unclutter.enable = true;
  services.xserver = {
    enable = true;
    xkb.layout = "ch";
    displayManager.lightdm.enable = true;
    windowManager.openbox.enable = true;
  };

  services.redshift.enable = true;
  location = {
    latitude = 47.519093;
    longitude = 8.017178;
  };

  # Fix i3lock-color after upgrading to nixpkgs 25.05
  security.pam.services.i3lock.enable = true;

  # Bluetooth
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # Modern headsets will generally try to connect using the A2DP profile.
    # https://nixos.wiki/wiki/Bluetooth
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
