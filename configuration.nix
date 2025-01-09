{ config, lib, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  # Boot configuration
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      timeout = 3;
    };
  };

  # Networking
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 80 443 ];
    };
  };

  # Nix settings
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # System
  time.timeZone = "Europe/Prague";
  system = {
    autoUpgrade = {
      enable = true;
      allowReboot = false;
    };
    stateVersion = "24.11";
  };

  # Localization
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "sun12x22";
    useXkbConfig = true;
  };

  # Hardware
  hardware = {
    bluetooth.enable = true;
  };

  # Power management
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };

  # Memory management
  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  # SSD optimization
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  # Location settings (used by redshift)
  location = {
    latitude = 50.0755;
    longitude = 14.4378;
  };

  # Security
  security = {
    rtkit.enable = true;
  };

  # Services
  services = {
    xserver = {
      enable = true;
      xkb = {
        layout = "us(colemak)";
        options = "caps:escape";
      };
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };

    libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
          tapping = true;
          scrollMethod = "twofinger";
        };
      };

    printing.enable = true;
    blueman.enable = true;
    
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    
    acpid.enable = true;
    
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
    };
    
    redshift = {
      enable = true;
      temperature = {
        day = 5500;
        night = 3700;
      };
    };
    
    upower.enable = true;
    udisks2.enable = true;
    throttled.enable = lib.mkDefault true;
    openssh.enable = true;
  };

  # Fonts
  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Noto Sans" ];
        monospace = [ "Fira Code" ];
      };
    };
  };

  # Users
  users.users.agheieff = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "network-manager"
      "audio"
    ];
  };

  # System packages (only system-wide tools)
  environment.systemPackages = with pkgs; [
    # Base utilities
    git
    wget
    xclip
    
    # System monitoring and management
    htop
    nixfmt-classic
    gnumake

    # X11 and window management
    dmenu
    rofi
    rofi-emoji
    xmonad-with-packages
    xmobar
    trayer
    nitrogen
    picom
    dunst
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras

    networkmanagerapplet

    pasystray
    pamixer
    pulseaudio
    alsa-utils

    blueman
    udiskie
    redshift

    brightnessctl
  ];

  # Programs
  programs = {
    mtr.enable = true;
    fish.enable = true;
    starship.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    dconf.enable = true;
  };
}
