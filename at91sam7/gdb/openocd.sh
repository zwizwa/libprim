#!/bin/bash
exec openocd --file interface/arm-usb-ocd.cfg  --file target/sam7x256.cfg --file $(dirname $0)/init.oocd

