{
	description = "My NixOS configuration";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		home-manager.url = "github:nix-community/home-manager";
		home-manager.inputs.nixpkgs.follows = "nixpkgs";
	};

	outputs = { self, nixpkgs, home-manager }: {
		nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			modules = [
				./configuration.nix
				#home-manager.nixosModules.home-manager
				#{
				#	home-manager.useGlobalPkgs = true;
				#	home-manager.useUserPackages = true;
				#	home-manager.users.agheieff = import ./home-manager.nix;
				#}
			];
		};
	};
}
