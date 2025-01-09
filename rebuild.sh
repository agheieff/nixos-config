#!/usr/bin/env bash

cd /etc/nixos
sudo cp -r ~/nixos/* /etc/nixos/
sudo nixos-rebuild switch --flake .#nixos
