# configuration.nix of Patrick Winter <patrickwinter@posteo.ch>

{ config, pkgs, ... }:


{
  imports = [
    /etc/nixos/work.nix  # Work sensitive settings which are not published
    /etc/nixos/host-configuration.nix
    /etc/nixos/hardware-configuration.nix
  ];

  # tmp
  networking.hosts = {
    "127.0.0.1" = [
        "ebau-t.so.ch"
        "camac-ng.local" "camac-ng-portal.local" "camac-ng-keycloak.local" "camac-ng-iweb-mock.local"
        "camac-be.local" "camac-be-keycloak.local"
        "caluma-portal.local"
        "e-learning.local" "chat.e-learning.local" "cloud.e-learning.local"
    ];
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
    plymouth.enable = true;
    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [ 22 ];
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Hack";
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
    tmux
    file
    git
    vim
    gnumake
    manpages
    jq
    htop
    strace
    bind
    fasd
    direnv
    fzf
    bat
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
    master.emacs  # Give me 27.1
    mu
    isync
    ripgrep
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

    # Diagrams
    graphviz
    pencil
    plantuml

    # Writing
    texlive.combined.scheme-full
    libreoffice
    pandoc
    haskellPackages.pandoc-citeproc
    pdftk
    pdfgrep

    # Remote file systems
    sshfs-fuse
    cifs_utils

    # For when I am traveling
    mosh
    rdesktop

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

    # Chat
    weechat
    discord

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
    networkmanager-openconnect
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
    spotify
    mpv
    vlc
    playerctl
    gimp
    gcolor3
    imagemagick
    ffmpeg
    zathura

    # Security
    gnupg
    pass
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
    nixops
    doctl

    # Emacs vterm
    libtool
    #libvterm  # Let emacs-libvterm clone the right version

    # C
    gdb
    gcc  # Also required to build python C extensions and vterm
    cmake

    # Python 3
    python37

    # Binaries
    python37Packages.pytest
    python37Packages.pytest-mock
    python37Packages.pytest-isort
    python37Packages.pytest-flake8
    python37Packages.pytest-black

    python37Packages.ipython
    python37Packages.black
    python37Packages.isort
    python37Packages.flake8

    python37Packages.python-language-server
    python37Packages.pyls-black
    python37Packages.pyls-isort

    python37Packages.ipython
    python37Packages.virtualenv
    python37Packages.virtualenvwrapper

    # JavaScript
    nodejs

    # Development tooling
    poedit
    cookiecutter

    # Communication
    unstable.mattermost-desktop
    slack
    unstable.zoom-us
    obs-studio
    mumble

    # Development
    gitAndTools.pre-commit

    # Collaboration
    tigervnc
    tmate

    # Nix awesomeness
    cachix
    nixfmt

    # Electronics
    # esptool
    # esptool-ck
    # arduino
    # adafruit-ampy
    # picocom

    # FHNW
    unstable.teams
    unstable.skype
    unstable.dotnet-sdk_3
    vscode
    omnisharp-roslyn
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
    extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "systemd-journal"];
  };

  # Use a dedicated user for pair programming
  users.users.pair = {
    isNormalUser = true;
    description = "User for tmux-based pair programming";
    openssh.authorizedKeys.keys = [
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5/xP7Ndt3iCwcO+P5Q1JCv52i3FKJBKmT8KdtmjL8EZgAHp+azhoHX0nxLgEvYpV1RfHdphpZfIN5Mi/80lg06NXeyuc+YOXTrvwYevW6+MdYhZ1EPCb1Idstck2BmDn1KAkfSNSa0GQYYHXXmAt7FFZsd5tEzUO7Xbe4wIvMbPR7csxcx3zncAJ7TmLeCt6vMDuYsYFC5gVKE6bPLfTpfo5+4e8qOHkjZKLRgHohYFW/AtFHL+vV59Ed+LMi/VZz9BqyGlwlK+3kOZkl6afX4Mu2ADi/arpeHXbb+VTH3YMijLyqoXbf/fVayiYMgwpySu31IO0c9p/HJDLPY5Pf jeanlf@srsyg20"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDXPOBnHo3ZGsniMzJH3SXkG9ZMf5ImRktPOq0OPlmSOHHG3GjaWaJnXGesGdOFS5Nn9dotQM7IgoAKDwSsi1Xb7wnrkCRLJ1n3FTg8CbD/Fwp1ksljBdZa9dmuI64qMl4SGNKr46aktxMKuT4cQVxVFjQ8eEhYukCWtp33f9R8Srt5MWagi29Y0DvPKKSNO8IuAfHe7n5Ag2qfQUT8QCKWMtFxa8GtaJCCwyYohvJihg/UiCdpKBEBs9QxFA46lw7sHdVNavWmhvnzf/esqp4OOFh0KhHaiDVIiiAMgJTzZ7jlvlNZ4wPMC0gNFYxPGWOjAa6D3iMvMGI9Lf+5+G21 lucasb"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNhknn+vnzZMbbfzN3BxxTkvZ7sKStHLUrVET1lJxazuiXap8B+1dhBWphH2pXDGKq+yIfCEYlQomBoZbkDPho/JfUQqk5yl3hcUz52kw/6ps8bv+XTH26RC1Q0wGmg39zqYt2drfV7UxwEcxKkriQWe2E0WmDWOPW8x9vvCasZ0rpQRm3OYIw4xIjfaUGR4Rn4uQmUYDqXqRJPtRiT2P0QIfGaBGmYK762uTPLD/a9il6nmAJukQUzp9oQu6DTlK/01Bh6o+1bWBWNo1O/ywF7J2lSZc5Lr4d8V8QOf68DblhRdmX7qrvPUlFND7LLyzdq1/AYZC3kxOsJFRdxJ0V olivers"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDaRathIJAr8CdKfuqcTyQEd2y5OWTK/s2J4UZpvvEgpIw+QIZh7xLFdF5CkBKVghsd1WeYOQmy9Y24PqPtDKxXB7qeNQOvgyVLGvpsdgZNlYKnh2nABVQgRWijMYFNJbDC9aVq/4rKF1bAKxStaULjciPsTaRBELF1cULNSGpcbLJYeFLHahPpXKFcJNfkHGXruO/34WOEptXwQfBDo328ThrGxKvVHgwpTN3FcfkEitGVH9719Y6wfhb/qhSbck2TkZ+PKbf7RZ+gwS7YHn6RB4YieZW3M+QhqUlqcBKtc2puqMpa8KlFz7TPMk7saVNaIrEA1v7QBSHSxZ8rLblX christianz"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCWlePj9VDxv236PwNMB+yEHPe6S3fA0f8OLPDwapwCb35YfprCBMrKWJz8O62RynXi0Cb3087KB8lePEKcrvmsuth+z071WdjmRsU7x36lonfJ3TFIi6vi+/vC9Mx2fHvCycKQqfY/rvL6bmvufRZC2l5BIh1s8ZpIaKdQJc4mREEVaYf8ybyEr/D3hu36NWn9rNGEiIRINNOrgmFRFJvtRlb/YuCa/5ZY5FOL968CAgqphER5CpJ0LzBvCKrBKegWdqUfgjtkkzmcEVRZo2GGH7Cu9tCHYMT0DRSTn0RHNaqj4tp5Hz2zDWVVLptJQYis5M/8HzWIVO7KJPFhN/SZ me"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAvYKpa8A4nw8aPzV3GyzJoGr4R2hM/fcJy/isDc6yTLlvpjulA69FOuN6zeqt5XbCCcLGXH6OM5JAFysCuR31vMOtrFxaoiOrFHPdOwm7sTTZQJmEEmYiHD/NwnrWLy8cSwAxOj0VeLaZSFXtyJrcG5Jss/VLtAiDQeWjQQMbSoBKS3ksOCobzVYYitLXu1OQMvwfkmaSE0CPMWecKjnpB9qVitA6CNxHyCFHB1IKrDyXMqbQg6qbM7d5SBzNAWDxHYgeAjRXW4VTTX166WoeDASt82zHPDCOFyddqfiBraKwLNl/LzniDtwrD1xOCNMuZ0KeuN5vN0Ksta5ENAZa0w== davidv"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDV+cJWbsztUuR8Is/jHOzSmNSdlfuZbpoFPjbslwkG4uiFaGMOARqA0dr+lp1j1H5/EpDsRv8bT5GwSmpblCWw8H6tCvFPzebDVTGINnuYBydq2XTKqmFqRJzj3ElXM8OL0cRUgjahkAOkX271zlD5FOK3ATURwOWq9H7Bj5O94DIsKk+rEK//BX6HD7XFaP/RaCqcEWFShaLoQ4YOQW4HU2UzBp1lNITtfAoyA+f9EzIScUqyzUe6DNWuRDklZo3pQbHfGrR+NB+6w4h6Csok0JnGEuRKaddHVW/20hNnJ2x46brEU0wFOlnBgxVWwAN+nTsH942BUpyRJosdBetf cyrillvw"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD20bPSV9FKM5X8dnJNo54FWKHdEUWKGeGnO/MRq8IPqzQHGJ3rWYzD/p+PDxQ0t1gX7P5+ZDlMhXqTKSMuiVcPPGrKfSolZci4oCbRIrf/k5fU8CperT0SIfIYwigXsHdW5njlQxSK/wbryrRbjuU519rO4QxbtKFZLGBhldRzZ20lJIGFD4qHJwUaya6Bi0dHgHBFmx4D/+eVxI27Ddrl2zgKMloSPLCCEggf6a+jDJnwRsIaEWGnseI+fFRwJQuETT17B8QkG9Iv6/Cty0lOmRAoHheJM9Odi2woTO4F86tk0KHwOFgzXtZEt798FsTubGEP2jxMGYvmEk30VMsJ jonasm"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDSScbcnfR5hfIXy2/rvupYiJeZLCepdwowprcG0dHXIMV9Anl7SbaxiLdeirSzLdeD4w1eQVTFYQF2GeWQzmreT9bXdYbNfEZE9QbEHaMnhxZSrdUjV+Jwc5RmomltCtK7Lb2LWFb1tNF5nhH8ECOQBpR2an0dT6ZpWQZoshl6A4MJl7plznnJGLho/MA24enr8HdcnASZ2HFSnSpiZRcxTvmHbaTjFBu/Xdlpsn4yyCKNRJhULg916VGYWIGgnpeHPEzmq6CeenVpBkDQ/TqnEJpffKVbd+SrAOLlv+S5Q00tPUN2ck15bWULPkSzzI1vbGXMBarxGKsjIq1PjnQWRBSeFicZqYmbdGVsMb7Xr7at0jm3P5ZTn5B75SdtMP42/ScOVu9rDVcb0Q5aXTsKKzIQ6375eqDnRcTCVaEDuJT+/KygXBIN7rhqOoMqss7JIVZLZTxWBVvyI5Zdd1ZNMOR1R+DEWOEWqWzgJMtmVz8y5MOd5SUyeBQh5RMd6tPlihpsAwzoNpKpd6B8jbh7vUqgTtWpFgIZwJrlfNkxlT/3vedwPrzPyvNun0Ncj3IjJgwNGAYklsGiRgRXX6upTibQ4DW/oj2Nv9FksLXaXX52RAjjeiBFY4mX8m81y6Q3cKbT1uch7olCkPL3WxBAJRV4LIMSLITtYriDhsleKQ== stefanb"
      "command=\"/run/current-system/sw/bin/tmux -S /tmp/tmux-pair attach -t pair\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDdi6gn/e5jMtuNjEy55j0cOcdnDBN16exkycZouB8UD29ki+s8JV4whD4qziC2qzKvTKFKmDSnqso1rCKnNGvlKosPwXUXY3uiBvULxoHVTt5kSpAn4gHIphTNt403LSnsf9QA1TIVscHE7CQZLLWBxNmPNXeLfRxG+euu2NaALNYgO1b1/ceevCZru4lWr3TvObVhh1athWH/fa9s2kDKb9g4jJiTu4NO6lA0TCO0Mrm60phqKgb8T6Pr5hTw10JVgkMi++bbn1JT6WuekZrmf6Ua0g0mZNwo2lPMfv9pDwzSRkEk6wd8u7Nh9udsPbxBckfCNfTbQtEPpVbmOmyvOc4ymX+zI8uvLFvS9mDD1e4XBx6s5Y5L61PdxVCidBzzevXx8DTHs57RIYR01a/H+1s6g3/x+rG0NcbX6B19TdHNMtDX1SrGiVlWeWsdYDI0Q3qfTW4mmPekFAiOtKnHUjEvVBSQ9+c4VzdgvAN2hEmVPjeeVa17T/5ArXIUlX6kMo5MTc0vCzEcweVTdrJs6AkmdQ6w9FDNeFB1FYCTOSrgc6wHokGE2bLPJZIlZjVHYJvFBr2ntiBMB217rwJt7vZclzMA3hghDzMKf4J3DpVf9daRcDH0AW1HJ8drCuM9H0ywl2xGJmACaVbJ/AmjOTHHwUnViSyNRCRn6ABVMw== fabioa"
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
    in ["${automount_opts},username=guest,password"];
  };

  fileSystems."/net/fhnw" = {
    device = "//fsemu18.edu.ds.fhnw.ch/e_18_data11$";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/home/patrick/.smb/fhnw.txt,vers=2.0"];
  };

  # Enable smart card reader
  services.pcscd.enable = true;

  # Start an emacs user service
  services.emacs = {
    enable = true;
    package = pkgs.master.emacs;
    defaultEditor = true;
  };

  virtualisation = {
    libvirtd.enable = true;
    docker.enable = true;
  };

  # Use composite manager for extra fanciness
  services.compton.enable = true;

  services.xserver = {
    enable = true;
    layout = "ch";
    displayManager.lightdm.enable = true;

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
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

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
