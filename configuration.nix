{ config, lib, pkgs, ... }:

{
	imports = [
		./hardware-configuration.nix
	];

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	networking.hostName = "nixos";
	networking.networkmanager.enable = true;

	nix.settings.experimental-features = [ "nix-command" "flakes" ];

	time.timeZone = "Europe/Prague";

# networking.proxy.default = "http://user:password@proxy:port/";
# networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

	i18n.defaultLocale = "en_US.UTF-8";
	console = {
		font = "sun12x22";
		useXkbConfig = true;
	};

	hardware = {
		bluetooth.enable = true;
	};

	security = {
		rtkit.enable = true;
	};

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
				day = 4600;
				night = 4600;
			};
			latitude = "0.0000000";
			longitude = "0.00000";
		};

		upower.enable = true;
		udisks2.enable = true;
	};



	users.users.agheieff = {
		isNormalUser = true;
		extraGroups = [
			"wheel"
			"network-manager"
			"audio"
		];
		packages = with pkgs; [
			tree
			firefox-devedition
			ungoogled-chromium
			transmission_4-gtk
			libreoffice-fresh
			gimp
			neovim
			emacs
			vlc
			telegram-desktop
			nchat

		];
	};

# List packages installed in system profile. To search, run:
# $ nix search wget
	environment.systemPackages = with pkgs; [
		python3
		redshift
		git
		alacritty
		xclip
		dmenu
		wget
		xmonad-with-packages
		xmobar
		trayer
		networkmanagerapplet
		pasystray
		blueman
		udiskie
		rofi
		brightnessctl
		pamixer
		pulseaudio
		alsa-utils
	];

	programs = {
		mtr.enable = true;
		gnupg.agent = {
			enable = true;
			enableSSHSupport = true;
		};
		dconf.enable = true;
	};

	services.throttled.enable = lib.
		mkDefault true;
	services.openssh.enable = true;

	networking.firewall.enable = false;

# Research before changing
	system.stateVersion = "24.11";

}

