{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.numtide.com" ];
    extra-trusted-public-keys = [ "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=" ];
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, nixos-hardware, llm-agents }: let
    system = "x86_64-linux";
    sharedModuleArgs = {
      _module.args.unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
      _module.args.llm-agents = llm-agents;
    };
  in {
    nixosConfigurations.tron = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
        ./configuration.nix
        ./hosts/tron
        ./hosts/tron/hardware.nix
        sharedModuleArgs
      ];
    };
    nixosConfigurations.clu = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ./configuration.nix
        ./hosts/clu
        ./hosts/clu/hardware.nix
        sharedModuleArgs
      ];
    };
  };
}
