{lib,...}:
with lib;
{
  config = {
    process.implementation = "process-compose";
    sites.landing-page.container-api.ports = mkAfter
      "- `9999`: Process Compose management API";
  };
}
