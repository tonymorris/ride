#!/bin/sh

rsync -aH --delete _site/ data:/var/www/ride.tmorris.net
