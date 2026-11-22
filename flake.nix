{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    elan.url = "github:ProfLander/elan";

    luajitSrc = {
      url = "github:LuaJIT/LuaJIT/v2.1";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, elan, luajitSrc, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      emacs = elan.packages.${system}.default;

      luajitVersion =
        "2.1.${pkgs.lib.strings.trim (builtins.readFile "${luajitSrc}/.relver")}";

      luajit = pkgs.luajit.overrideAttrs (_: {
        version = luajitVersion;
        src = luajitSrc;
      });

      love = (pkgs.love.override {
        inherit luajit;
      }).overrideAttrs (_: {
        version = "11.5";
        src = pkgs.fetchFromGitHub {
          owner = "love2d";
          repo = "love";
          rev = "11.5";
          hash = "sha256-wZktNh4UB3QH2wAIIlnYUlNoXbjEDwUmPnT4vesZNm0=";
        };
      });

      runtimePackages = [
        pkgs.racket
        luajit
        love
      ];

      emacsWithRuntime = pkgs.symlinkJoin {
        name = "emacs-with-runtime";
        paths = [ emacs ];

        nativeBuildInputs = [ pkgs.makeWrapper ];

        postBuild = ''
          wrapProgram "$out/bin/emacs" \
            --prefix PATH : "${pkgs.lib.makeBinPath runtimePackages}"
        '';

        meta.mainProgram = "emacs";
      };
    in
    {
      packages.${system} = {
        default = emacsWithRuntime;
        inherit luajit love;
      };

      devShells.${system}.default = pkgs.mkShell {
        packages = [ emacs ] ++ runtimePackages;
      };
    };
}
