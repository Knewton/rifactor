# rifactor

AWS Reserved Instance Optimization

## Prerequisites

- GHC 7.10
- cabal-install 1.22

## Build

[![Build Status](https://travis-ci.org/dysinger/rifactor.png)](https://travis-ci.org/dysinger/rifactor)

    make

## Test

    make test

## Install

    install ./.cabal-install/bin/rifactor /usr/local/bin

## Run

    rifactor --help

## Config File (Required)

```json
{
  "accounts": [
    {
      "access_key": "<<AWS_ACCESS_KEY_ID_HERE>>",
      "secret_key": "<<AWS_SECRET_ACCESS_KEY_HERE>>",
      "name": "dev"
    },
    {
      "access_key": "<<AWS_ACCESS_KEY_ID_HERE>>",
      "secret_key": "<<AWS_SECRET_ACCESS_KEY_HERE>>",
      "name": "qa"
    },
    {
      "access_key": "<<AWS_ACCESS_KEY_ID_HERE>>",
      "secret_key": "<<AWS_SECRET_ACCESS_KEY_HERE>>",
      "name": "stage"
    },
    {
      "access_key": "<<AWS_ACCESS_KEY_ID_HERE>>",
      "secret_key": "<<AWS_SECRET_ACCESS_KEY_HERE>>",
      "name": "prod"
    }
  ],
  "regions": [
    "NorthCalifornia",
    "NorthVirginia",
    "Oregon"
  ]
}
