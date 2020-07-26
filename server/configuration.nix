# This is a configuration file for new servers to run Corpus-DB.

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda"; # Vultr
  };

  networking.hostName = "corpus-db-nixos"; # Define your hostname.

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    # Standard CLI tools
    wget vim fish git 
    # Fancy CLI tools 
    fd bat ncdu gotop
    # Haskell development
    ghc stack
    # Other
    libxml2 sqlite sqlite-interactive
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs = {
    fish = {
      enable = true;
    };
    tmux = { 
      enable = true;
    };
  };

  # Enable the OpenSSH daemon.
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
    };
    fail2ban = {
      enable = true;
    };
  };

# This isn't working right now because the service seems to time out. 
# I think I have to change something in the haskell to have it return a successful code
#  systemd.services.corpus-db = {
#     description = "Corpus DB Webserver";
#     enable = true;
#     environment = { ENV = "prod"; };
#     serviceConfig = {
#       Type = "forking";
#       ExecStart = "/home/jon/corpus-db/result/bin/corpus-db";
#       ExecStop = "/run/current-system/sw/bin/pkill corpus-db"; 
#       #Restart = "on-failure";
#       #StartLimitInterval = 20;
#     };
#     wantedBy = [ "default.target" ];
#   };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 80 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Can't get this to work. See: 
  # https://discourse.nixos.org/t/how-do-i-set-up-a-swap-file/8323/3
  # swapDevices = [ { device = "/var/swap"; size= 1024; } ]; 

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jon = {
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAuEaxTj1/UVvyXMOMq354epTabzXPPqRCQDlo/vXXHQgqZ9dd4lTU/ol5g59Rmd80WFHvzJFSieJ1a0weXJ8wu9xY6gbjitGaKPlyFQZfwynXeC8jwTRDgih5fjYXBbnIRtRvpSiXkC+jAH019UbgiFRr9Fg5g582iFpXYiIpa2dLnXRs0Sz6sbzoeJL0t566Zt/s8QvfBfzlXvM9AFkHdO+Z88LS4Hh8BN75+9tpkrrQQNOium2gqhHKGpCP0Xf6zPVYJYfpGFOhjKYnl2jihAwHLPHb+dcGdq4Uyj59SEJBtsRPvu3+82X8vFdUmdE22uTzFaw8JJ+rNuYfiAf/tw== jon.reeve@gmail.com" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDqm3Uw8BKvlpTxvcPFUYt3uQ8V72odts2hqStl7CgZ8G4hXlQIc6m1BWaePq1beRCIHEj+h4Of5XiA/nsUk080ff6FwTM6i82P4TE59sbn4Qwtwu/+xNHUO6j3kfIRhR3amIsEeRdpDaX42YvVqVtquCNHQmcqeTSNqfwUKkZKP51tNqvGumPGbtcnQEYEeGOrOv0LOQ4YC83zjnOSYuWfwZ9QxI0FNi9QGG61BtZWmv2pML+AjuGKwaXQsGkFk2Z0JYCyQdYYeOq6jWrefdAAzbPUN9p8QSP5890tS7GgC9f8yQCspz7Ru92/9JO7pM9CthF/PLYIQHa7YIUvLNBN jon@jon-laptop"  ];

    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
