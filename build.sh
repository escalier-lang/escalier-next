#!/usr/bin/env bash
dotnet publish src/Escalier.Playground -c Release --self-contained -o out
if [ ! -e "out/wwwroot/node_modules" ]; then
  pushd out/wwwroot || exit 1
    ln -s ../../node_modules node_modules
  popd || exit 1
fi
