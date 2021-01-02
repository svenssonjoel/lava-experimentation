#!/bin/sh

echo "Setting up Lava2021"


if [ -d $HOME/.lava ]
then
    echo "$HOME/.lava exists"
else
    echo "Creating $HOME/.lava"
    mkdir $HOME/.lava
fi


if [ ! -d $HOME/.lava/Scripts ]
then
    echo "Creating $HOME/.lava/Scripts"
    cp -r Scripts $HOME/.lava
fi

