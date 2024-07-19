# escalier-next

[![CI](https://github.com/escalier-lang/escalier/actions/workflows/ci.yml/badge.svg)](https://github.com/escalier-lang/escalier/actions/workflows/ci.yml)
[![codecov](https://codecov.io/github/escalier-lang/escalier-next/graph/badge.svg?token=XL2SFYNEID)](https://codecov.io/github/escalier-lang/escalier-next)

This is a rewrite of [Escalier](http://github.com/escalier-lang/escalier) in
F#. The hope is the F# will make it easier to develop Escalier.

## Development

Fixture tests can be updated by running the following command:
```
ESCALIER_UPDATE_FIXTURES=1 dotnet test src/Escalier.Compiler.Tests
```