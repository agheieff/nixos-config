{ config, pkgs, ... }:

{
  home.username = "agheieff";
  home.homeDirectory = "/home/agheieff";
  home.stateVersion = "24.11";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # Start with just packages, no config management
  home.packages = with pkgs; [
    alacritty
    neovim
    firefox-devedition
    ungoogled-chromium
    transmission_4-gtk
    libreoffice-fresh
    gimp
    vlc
    telegram-desktop
    nchat
  ];

  xdg.configFile."xmonad/xmonad.hs".source = ./xmonad.hs;
}
