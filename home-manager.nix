{ config, pkgs, ... }:

{
	home.username = "agheieff";
	home.homeDirectory = "/home/agheieff";
	home.packages = with pkgs; [
	];

	home.stateVersion = "24.11";

	programs.bash.enable = true;
	programs.git = {
		enable = true;
		username = "agheieff";
		userEmail = "agheieff@pm.me";
	};
}
