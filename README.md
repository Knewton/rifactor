# rifactor

AWS Reserved Instance Optimization

## Build

You need GHC 7.8.x & cabal-install 1.22 (homebrew or PPA will work). Take
a peek at the .travis.yml file at the root of the project for build steps.

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
```

## IAM Permissions

Create a new IAM.  Add a User Policy that allows for describing ec2
resources & modifying reserved instances.

"ec2-describe-and-modify-reserved-instance"

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": "ec2:Describe*",
      "Resource": "*"
    },
    {
      "Effect": "Allow",
      "Action": "ec2:ModifyReservedInstances",
      "Resource": "*"
    }
  ]
}
```
