#!/usr/bin/env bash

sudo cp -r ~/.config/nixos/* /etc/nixos/
sudo nixos-rebuild switch
