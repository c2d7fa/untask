let
  pkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "nixOS";
    repo = "nixpkgs";
    rev = "98747f27ecfee70c8c97b195cbb94df80a074dda";
    sha256 = "R7jKS7A+0tZS8qD5pBr1UFcMiTdsw5bfoxgXbYsoWhM=";
  }) {};
  appimagetool = pkgs.appimageTools.wrapType2 {
    name = "appimagetool";
    src = pkgs.fetchurl {
      url = "https://github.com/AppImage/AppImageKit/releases/download/13/appimagetool-x86_64.AppImage";
      sha256 = "3zuvXKX6y+z8Lz+mcTwpq5zvqP2MHqxdKDt5yrM+Sss=";
    };
    extraPkgs = pkgs: [pkgs.file];
  };
in pkgs.mkShell {
  buildInputs = [ pkgs.racket appimagetool pkgs.earthly ];
}


