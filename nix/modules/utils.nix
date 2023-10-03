# This module contains some CLI utilities that are useful to have in the container
{pkgs, ...}:
{
  packages = [
    pkgs.curl # Used by some CI automation
  ];
}