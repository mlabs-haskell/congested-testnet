# or local testing virtual machines
{ lib, modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  # WARNING: root access with empty password
  # provides easy debugging via console and ssh
  services.getty.autologinUser = "root";
  services.openssh.settings.PasswordAuthentication = lib.mkForce true;
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
  services.sshd.enable = true;
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
      {
        from = "host";
        host.port = 13000;
        guest.port = 3000;
      }
      {
        from = "host";
        host.port = 11442;
        guest.port = 1442;
      }
      {
        from = "host";
        host.port = 19090;
        guest.port = 9090;
      }
      {
        from = "host";
        host.port = 18000;
        guest.port = 8000;
      }
      {
        from = "host";
        host.port = 12789;
        guest.port = 12789;
      }
      {
        from = "host";
        host.port = 18001;
        guest.port = 8001;
      }
      {
        from = "host";
        host.port = 15000;
        guest.port = 5000;
      }
    ];
  };
}
