{
  inputs.nixpkgs.url = github:NixOs/nixpkgs;
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.zsh-autosuggestions = {
    url = github:zsh-users/zsh-autosuggestions;
    flake = false;
  };

  inputs.nix-zsh-completions = {
    url = github:spwhitt/nix-zsh-completions;
    flake = false;
  };

  inputs.zsh-syntax-highlighting = {
    url = github:zsh-users/zsh-syntax-highlighting;
    flake = false;
  };

  outputs = {
    zsh-autosuggestions, nix-zsh-completions, zsh-syntax-highlighting, nixpkgs, flake-utils, ...}:

  flake-utils.lib.eachDefaultSystem (system: 
  let
    pkgs = nixpkgs.legacyPackages.${system};
    inherit (pkgs) stdenv lib;

    actualize = stdenv.mkDerivation {
        name = "actualize-dotfiles";

        installPhase = ''
          mkdir -p $out/bin
          cp -R ${./files} $out/files

          cat >$out/bin/actualize-dotfiles <<EOF
          #!${pkgs.bash}/bin/bash

          ln -sTf ${./vimrc} ~/.vimrc
          ln -sTf ${./vim.conf.modules} ~/.vim.conf.modules

          mkdir -p ~/.config/nvim/
          ln -sTf ~/.vimrc ~/.config/nvim/init.vim
          ln -sTf ${./coc-settings.json} ~/.config/nvim/coc-settings.json
          mkdir -p ~/.vim
          ln -sTf ${./coc-settings.json} ~/.vim/coc-settings.json
          mkdir -p ~/.vim/after/syntax
          ln -sTf ${./bytecode.vim} ~/.vim/after/syntax/bytecode.vim

          ln -sTf ${./zshrc} ~/.zshrc
          mkdir -p ~/.zsh
          ln -sTf ${./default-keybinds.zsh} ~/.zsh/default-keybinds
          ln -sTf ${zsh-autosuggestions} ~/.zsh/zsh-autosuggestions
          ln -sTf ${zsh-syntax-highlighting} ~/.zsh/zsh-syntax-highlighting
          ln -sTf ${nix-zsh-completions} ~/.zsh/nix-zsh-completions

          mkdir -p ~/.clojure
          ln -sTf ${./.clojure/deps.edn} ~/.clojure/deps.edn

          for file in \$(ls $out/files -A); do
            ln -sTf $out/files/\$file ~/\$file
          done

          # protect dotfiles from gc
          nix-store --add-root ~/.keep-dotfiles -r $out

          EOF

          chmod +x $out/bin/actualize-dotfiles
          '';

          phases = ["installPhase"];
      };

    scripts = stdenv.mkDerivation {
      name = "dotfiles-scripts";
      dontUnpack = true;
      buildInputs = [ pkgs.python3 ];
      installPhase =  ''
          mkdir -p $out/bin
          cp ${./scripts}/* $out/bin

          python ${./render-shebang.py} \
            --interpreter ${pkgs.bash}/bin/bash \
            --interpreter ${pkgs.bash}/bin/sh \
            --interpreter ${pkgs.python3}/bin/python \
            --interpreter ${pkgs.python3}/bin/python3 \
            --interpreter ${pkgs.zsh}/bin/zsh \
            $out/bin/*

          chmod +x $out/bin/*

          ln -s ${actualize}/bin/actualize-dotfiles $out/bin
      '';
    };
  in
   {
      packages.default = actualize;
      packages.actualize-dotfiles = actualize;
      packages.dotfiles-scripts = scripts;
   });
}
