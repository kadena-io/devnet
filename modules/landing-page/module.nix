{pkgs, ...}:
{
  config = {
    services.http-server.servers.devnet.extraConfig = ''
      location / {
        alias ${pkgs.runCommand "landing-page" {} ''
          mkdir -p $out
          ${pkgs.pandoc}/bin/pandoc --template=${./index.html} -s ${./index.md} -o $out/index.html
          ln -s ${./static} $out/static
        ''}/;
      }
    '';
  };
}