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
    zsh-autosuggestions, nix-zsh-completions, zsh-syntax-highlighting,
    nixpkgs, flake-utils, ...}:
  flake-utils.lib.eachDefaultSystem (system: 
  let
    pkgs = nixpkgs.legacyPackages.${system};
    inherit (pkgs) stdenv lib;
    actualize = stdenv.mkDerivation {
        name = "actualize-dotfiles";
        dontUnpack = true;
        sourceRoot = ".";
        installPhase = ''
          mkdir -p $out/bin

          cat >$out/bin/actualize-dotfiles <<EOF
          #!${pkgs.bash}/bin/bash

          ln -sTf ${./.vimrc} ~/.vimrc
          ln -sTf ${./.vim.conf.modules} ~/.vim.conf.modules

          mkdir -p ~/.config/nvim/
          ln -sTf ~/.vimrc ~/.config/nvim/init.vim
          ln -sTf ${./coc-settings.json} ~/.config/nvim/coc-settings.json
          mkdir -p ~/.vim
          ln -sTf ${./coc-settings.json} ~/.vim/coc-settings.json
          mkdir -p ~/.vim/after/syntax
          ln -sTf ${./.vim/after/syntax/bytecode.vim}

          ln -sTf ${./.zshrc} ~/.zshrc
          mkdir -p ~/.zsh
          ln -sTf ${./.zsh/default-keybinds} ~/.zsh/default-keybinds
          ln -sTf ${zsh-autosuggestions} ~/.zsh/zsh-autosuggestions
          ln -sTf ${zsh-syntax-highlighting} ~/.zsh/zsh-syntax-highlighting
          ln -sTf ${nix-zsh-completions} ~/.zsh/nix-zsh-completions

          ln -sTf ${./.alacritty.yml} ~/.alacritty.yml
          ln -sTf ${./.tmux.conf} ~/.tmux.conf
          ln -sTf ${./.gdbinit} ~/.gdbinit
          ln -sTf ${./.gitconfig} ~/.gitconfig
          ln -sTf ${./.ideavimrc} ~/.ideavimrc

          mkdir -p ~/.clojure
          ln -sTf ${./.clojure/deps.edn} ~/.clojure/deps.edn

          EOF

          cp ${./scripts}/* $out/bin

          chmod +x $out/bin/actualize-dotfiles
          '';
      };
  in
   {
      packages.default = actualize;
      packages.actualize-dotfiles = actualize;
   });
}
