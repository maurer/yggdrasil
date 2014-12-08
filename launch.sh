#!/bin/bash
mkdir -p /var/yggdrasil
cp -r static /var/yggdrasil/
dist/build/yggdrasil/yggdrasil Production
