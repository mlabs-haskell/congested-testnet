{ config, pkgs, lib, ... }:

{
  # Enable the X11 windowing system
  services.xserver.enable = true;

  # Select a desktop environment or window manager
  services.xserver.desktopManager.xfce.enable = true; # Example: XFCE Desktop

  # Enable LightDM and set automatic login
  services.xserver.displayManager.lightdm = {
    enable = true;
    autoLogin = {
      enable = true;
      user = "yourUsername"; # Replace with your username
    };
  };

  # Network configuration (example: enable network manager)
  networking.networkmanager.enable = true;

  # Set your system’s hostname
  networking.hostName = "yourHostname"; # Replace with your hostname

  # Define user
  users.users.yourUsername = { # Replace with your username
    isNormalUser = true;
    createHome = true;
    home = "/home/yourUsername"; # Replace with your home directory path
    description = "Your User";
    extraGroups = [ "wheel" ]; # Add additional groups if needed
    password = "yourPassword"; # Replace with your password
  };

  # Set the default system timezone
  time.timeZone = "UTC";

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    # Add your packages here
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
}
