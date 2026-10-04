{
  inputs = {
    nixpkgs.url = "git+https://mirrors.nju.edu.cn/git/nixpkgs.git?ref=nixpkgs-unstable&shallow=1";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {

          # Typically, you just want a single project named "default". But
          # multiple projects are also possible, each using different GHC version.
          haskellProjects.default = {
            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://zero-to-flakes.com/haskell-flake/package-set
            basePackages = pkgs.haskellPackages;

            # Extra package information. See https://zero-to-flakes.com/haskell-flake/dependency
            #
            # Note that local packages are automatically included in `packages`
            # (defined by `defaults.packages` option).
            #
            #packages = {
            #  pacakge.source = "";
            #};
            settings = {
            };

            devShell = {
              # Enabled by default
              enable = true;

              mkShellArgs = {
                # 将 nodejs、npm 等加入 shell 环境
                nativeBuildInputs = with pkgs; [
                  nodejs
                  # 你还可以添加其他工具，例如 pnpm、yarn
                  # nodePackages.pnpm
                  # nodePackages.yarn
                ];

                # 可选：添加 shellHook 来验证环境或设置变量
                shellHook = ''
                  export PATH="$PWD/node_modules/.bin/:$PATH"
                  export NPM_PACKAGES="$PWD/.npm-packages"
                '';
              };

              # Programs you want to make available in the shell.
              # Default programs can be disabled by setting to 'null'
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

              hlsCheck.enable = true;
            };
          };

          # haskell-flake doesn't set the default package, but you can do it here.
          packages.default = self'.packages.example;
        };
    };
}
