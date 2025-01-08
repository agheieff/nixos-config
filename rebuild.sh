#!/usr/bin/env bash

# Ensure we're in the right directory
cd /etc/nixos

# Copy configuration files
sudo cp -r ~/nixos/* /etc/nixos/

# Rebuild the system using flakes
sudo nixos-rebuild switch --flake .#nixos
