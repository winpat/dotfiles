# configuration.nix of Patrick Winter <patrickwinter@posteo.ch>

{ config, pkgs, ... }:


{
  imports = [
    /etc/nixos/work.nix  # Work sensitive settings which are not published
    /etc/nixos/host-configuration.nix
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/cachix.nix
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;}))
    (self: super: {
      # https://github.com/emilypeto/openbox-window-snap
      openbox = super.openbox.overrideAttrs  (oldAttrs: rec {
        patches = [ ./openbox-aerosnap.patch ];
      });
    })
  ];

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
    plymouth.enable = true;
    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [ 22 ];
    hosts = {
      "192.168.1.2" = ["znc.winpat.ch"];
    };
  };

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
    enableDefaultFonts = true;
    fonts = with pkgs; [ hack-font font-awesome ];
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
    manpages
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
    plan9port

    # Networking
    nmap
    telnet
    netcat

    # HTTP
    wget
    curl
    httpie

    # Emacs
    emacs
    mu
    isync
    sqlite  # Forge

    # Spell checking
    hunspell
    hunspellDicts.de-ch
    hunspellDicts.en-us-large

    # Desktop
    termite
    dmenu
    xcape
    xplanet
    xorg.xev
    xlibs.xmodmap
    xclip
    xsel
    xdotool
    wmctrl
    feh
    arandr
    libnotify
    dunst
    flameshot  # scrot on steroids
    udiskie
    gparted
    acpi

    # i3
    i3-layout-manager
    python37Packages.i3ipc
    perl
    i3blocks

    # openbox
    tint2
    obconf

    # Diagrams
    graphviz
    plantuml
    xmind

    # Writing
    texlive.combined.scheme-full
    libreoffice
    pandoc
    haskellPackages.pandoc-citeproc
    pdftk
    poppler_utils
    pdfgrep

    # Remote file systems
    sshfs-fuse
    cifs_utils

    # For when I am traveling
    mosh

    # Misc Desktop Applications
    dmidecode
    pciutils
    binutils
    usbutils
    pmutils
    cups
    pavucontrol

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
    unstable.discord
    quasselClient
    signal-desktop
    tdesktop
    slack
    unstable.zoom-us
    teams


    # Databases
    pgcli

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
    wireguard

    # Fix missing icon for networkmanageapplet
    # https://github.com/NixOS/nixpkgs/issues/32730
    hicolor-icon-theme

    # Virtualization and containers
    virt-viewer
    virtmanager
    vagrant
    docker
    docker_compose

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
    pwgen
    yubikey-personalization
    pinentry_gnome

    # File synchronization
    rsync
    unison

    # Clipboard manager
    copyq

    # Ergodox Ez
    wally-cli

    # Automation
    ansible
    terraform
    vault
    packer
    consul
    nixops
    doctl

    # C
    gdb
    gcc
    cmake

    # Python 3
    python3

    # Binaries
    python3Packages.pytest
    python3Packages.pytest-mock
    python3Packages.pytest-isort
    python3Packages.pytest-flake8
    python3Packages.pytest-black
    python3Packages.pytest-mypy
    python3Packages.pytestcov

    python3Packages.ipython
    python3Packages.bpython

    python3Packages.ipdb

    python3Packages.black
    python3Packages.isort
    python3Packages.flake8
    python3Packages.mypy
    python3Packages.coverage

    unstable.python3Packages.python-language-server
    python3Packages.pyls-black
    python3Packages.pyls-isort
    python3Packages.pyls-mypy

    python3Packages.virtualenv
    python3Packages.virtualenvwrapper

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
    # TODO These appear broken
    # esptool
    # esptool-ck
    arduino
    adafruit-ampy
    picocom

    # GCP
    google-cloud-sdk
    kubectx
    kubectl

    # Misc
    asciiquarium
    # unstable.cbonsai
  ];

  services.udev.packages = with pkgs; [
    yubikey-personalization
    libu2f-host
  ];

  # Add additional udev rules for Ergodox EZ and Planck EZ
  services.udev.extraRules = ''
    # Teensy rules for the Ergodox EZ Original / Shine / Glow
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

    # STM32 rules for the Planck EZ Standard / Glow
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
        MODE:="0666", \
        SYMLINK+="stm32_dfu"
  '';

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
    permitRootLogin = "yes";
  };

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
    device = "//192.168.1.2/media";
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
    package = pkgs.emacs;
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
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
    windowManager.openbox.enable = true;

    # Lock the screen after 60 seconds of inactivity
    xautolock = {
      enable = true;
      locker = "${pkgs.i3lock-color}/bin/i3lock-color -k  -c 000000 --timecolor ffffffff --datecolor ffffffff --datestr='%d/%m/%Y'";
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

  # Use `nix run nixpkgs.hplipWithPlugin -c sudo hp-setup`. For further
  # information checkout: https://nixos.wiki/wiki/Printing
  services.printing = {
    enable = true;
    drivers = [
      pkgs.hplip
      pkgs.hplipWithPlugin
    ];
  };

  # Required to run steam:
  # https://github.com/NixOS/nixpkgs/issues/47932
  #hardware.opengl.driSupport32Bit = true;
  #hardware.pulseaudio.support32Bit = true;

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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
