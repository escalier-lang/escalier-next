#!/usr/bin/env bash
rm -rf out
dotnet publish src/Escalier.Playground -c Release --self-contained -o out
pushd out || exit 1
  mv wwwroot escalier-next
  pushd escalier-next || exit 1
    mkdir -p node_modules/typescript/lib
    cp ../../node_modules/typescript/package.json node_modules/typescript
    cp ../../node_modules/typescript/lib/*.d.ts node_modules/typescript/lib
    mkdir types
    cp ../../types/*.d.ts types
  popd || exit 1
popd || exit 1
