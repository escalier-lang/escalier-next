#!/usr/bin/env bash
rm -rf out
dotnet publish src/Escalier.Playground -c Release --self-contained -o out
pushd out || exit 1
  mv wwwroot escalier-next
popd || exit 1
if [ ! -e "out/escalier-next/node_modules" ]; then
  pushd out/wwwroot || exit 1
    ln -s ../../node_modules node_modules
  popd || exit 1
fi
