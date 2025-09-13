#!/bin/bash

sudo ip link add name service type bridge
sudo ip addr add 10.0.0.1/24 dev service
sudo ip tuntap add name tap0 mode tap
sudo ip link set tap0 master service
sudo ip link set service up
sudo ip link set tap0 up
