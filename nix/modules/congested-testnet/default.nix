{ lib, pkgs, modulesPath, ... }:
{
  imports = [ 
    # "${modulesPath}/virtualisation/digital-ocean-config.nix" 
    ./services.nix
  ];

  boot.initrd.kernelModules = [ "nvme" ];
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];

  boot.loader.grub.device = "/dev/vda";

  fileSystems."/" = {
    device = lib.mkForce "/dev/vda1";
    fsType = "ext4";
  };

  system.stateVersion = "23.11";

  # virtualisation.docker.enable = false;
  # virtualisation.podman.enable = true;
  # virtualisation.podman.dockerSocket.enable = true;
  # virtualisation.podman.defaultNetwork.settings.dns_enabled = true;
  environment.systemPackages = [
    # pkgs.docker-client
    # pkgs.dnsutils
    # pkgs.docker
    # pkgs.arion-with-prebuilt
    pkgs.htop
    # pkgs.cardano-node
  ];

  networking.hostName = "congested-testnet";

  networking.firewall.interfaces.podman1.allowedUDPPorts = [ 53 ];
  networking.firewall.allowedTCPPorts = [ 1337 9090 1442 3000 5000 8000 8001 8002 ];


  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzCmDCtlGscpesHuoiruVWD2IjYEFtaIl9Y2JZGiOAyf3V17KPx0MikcknfmxSHi399SxppiaXQHxo/1wjGxXkXNTTv6h1fBuqwhJE6C8+ZSV+gal81vEnXX+/9w2FQqtVgnG2/mO7oJ0e3FY+6kFpOsGEhYexoGt/UxIpAZoqIN+CWNhJIASUkneaZWtgwiL8Afb59kJQ2E7WbBu+PjYZ/s5lhPobhlkz6s8rkhItvYdiSHT0DPDKvp1oEbxsxd4E4cjJFbahyS8b089NJd9gF5gs0b74H/2lUUymnl63cV37Mp4iXB4rtE69MbjqsGEBKTPumLualmc8pOGBHqWIdhAqGdZQeBajcb6VK0E3hcU0wBB+GJgm7KUzlAHGdC3azY0KlHMrLaZN0pBrgCVR6zBNWtZz2B2qMBZ8Cw+K4vut8GuspdXZscID10U578GxQvJAB9CdxNUtrzSmKX2UtZPB1udWjjIAlejzba4MG73uXgQEdv0NcuHNwaLuCWxTUT5QQF18IwlJ23Mg8aPK8ojUW5A+kGHAu9wtgZVcX1nS5cmYKSgLzcP1LA1l9fTJ1vqBSuy38GTdUzfzz7AbnkRfGPj2ALDgyx17Rc5ommjc1k0gFoeIqiLaxEs5FzDcRyo7YvZXPsGeIqNCYwQWw3+U+yUEJby8bxGb2d/6YQ== andreaciceri@mlabs.city"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDKvzcswvJ4iXTpp2sT78/dldAox07fS47qVOZeZxS9aDLB1GugyBciW0ps6THZzqr3w1H2IjJKUppjavDKc54U6W3Bt84NJwOaoFYDtCBPRs+2dQqdk68E+3NO4mw7V2fdk5qiIvJKubF5PrdiYzsoGe9G4i/QDOPts51VER1b5wNcs7hF36FjRxm7HgyVybuG5S8fVTl8KcKeZiy4arPCyqMsnwgmsIXeUg0uXx2ZKYmkZVx+gu5GeJyZqjOJb/t0ujbU1DFa7q3d1rkI5+BhY5/kCLEBnZGuDrt1S8w5m7OVqIV0fogVEt2wFL0YKTwkkh8i6nZ+bKQTG7H9apkZvsTitIef3V/z8ajwckiBarD1NJJKtsEczJVcohfVFVr8hcXs+bqnIMRZgulJzrjp4DyPsHNd6lTB+ASLY6hQMAdoD4elR18HtyF8AEZFw+3MTWWn4YOTAY+cmdm0nNFlo8BJnYgrqCsaeWbaKOTvnPj+rSpkNw+zD7MqXhHESkM= maxim@maxim"
  ];


}
