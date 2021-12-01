let
  pkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "nixOS";
    repo = "nixpkgs";
    rev = "98747f27ecfee70c8c97b195cbb94df80a074dda";
    sha256 = "R7jKS7A+0tZS8qD5pBr1UFcMiTdsw5bfoxgXbYsoWhM=";
  }) {};
in pkgs.mkShell {
  buildInputs = [ pkgs.racket pkgs.earthly ];
}


