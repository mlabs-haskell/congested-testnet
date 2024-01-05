# or local testing virtual machines
{ lib, modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  # WARNING: root access with empty password
  # provides easy debugging via console and ssh
  services.getty.autologinUser = "root";
  services.openssh.settings.PasswordAuthentication = lib.mkForce true;
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
  users.users.root.password = "";

  # Virtual Machine configuration
  virtualisation = {
    graphics = false;
    # memorySize = 8000;
    # diskSize = 10000;
    forwardPorts = [
      {
        from = "host";
        host.port = 2222;
        guest.port = 22;
      }
      {
        from = "host";
        host.port = 11337;
        guest.port = 1337;
      }
    ];
  };
}
