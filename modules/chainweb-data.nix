{ pkgs, ...}:
let
in
{
  config = {
    services.postgres.enable = true;
  };
}