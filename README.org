#+TITLE: RIFactor
#+STARTUP: content noindent odd hidestars hideblocks
#+OPTIONS: toc:nil

[[https://travis-ci.org/Knewton/rifactor.png]]

* Automatically refactor AWS Reserved Instances to match running Instances

  Companies purchase Amazon AWS Reserved Instances as a way to cut
  cost on their Instances.  When you first purchase blocks of reserved
  instances, you must pick an Instance Type, Network Type &
  Availability Zone.

  [[./docs/initial.png]]

  As long as you never change your running Instances, you will save
  money.  If you change your instances, you may end up paying double.
  You'll pay on-demand pricing for the new Instances AND you will
  still pay for the Reserved Instances you purchased.

  [[./docs/reality.png]]

  Over time, as you terminate & re-launch Instances, your Reserved
  Instances may no longer match your running Instances.  We'll need to
  reconfigure your Reserved Instances so they match your new
  configuration so you aren't paying double.

  [[./docs/after.png]]

  Getting this just right across all your accounts & machines is
  tedious. Luckily computers are good at the tedious stuff.  RIFactor
  will (read-only) probe your Amazon account for EC2 Instance &
  Reserved Instances information.  It then does three transformations
  of the Manifest in order:

  1. RIFactor matches running Instances with the current Reserved
     Instances.

     - Input
       1. Instances
       2. Reserved Instances

     - Output
       1. Instances (unmatched)
       2. Reserved Instances (unmatched)
       3. Used Reserved Instances (partial or full)

  2. RIFactor will then combine empty Reserved Instances together.

     - Input
       1. Reserved Instances (unmatched)

     - Output
       1. Reserved Instances (unmatched)
       2. Combined Reserved Instances

  3. RIFactor will then split your Reserved Instances and Used
     Reserved Instances up to fit remaining Instances.

     - Input
       1. Instances (unmatched)
       2. Reserved Instances (unmatched)
       3. Used Reserved Instances (partially used but not to capacity)

     - Output
       1. Instances (unmatched)
       2. Reserved Instances (unmatched)
       3. Used Reserved Instances (partial or full)
       4. Split Reserved Instances (partial or full)
       5. Split Used Reserved Instances (partial or full)

  Once RIFactor has probed Amazon for data & ran the Manifest through
  these transformations, it can show you the plan and execute.

  NOTE: The purpose of combining Reserved Instances back together
  again is to optimize applicability. We want, where possible, the
  Reserved Instances to be in as big as they can be so we can apply
  them to the most Instances we can.  This counteracts our splitting
  them up again (to fit new node sizes.)

  NOTE: RIFactor considers all of your accounts listed in the config
  file to be linked together on Amazon's billing.  If you have related
  accounts such as a billing account (with the Reserved Instances in
  it) and other working accounts with the Instances running in them,
  the app will consider all of them as a whole & try to apply the best
  Reserved Instance reconfiguration.  If you have many unrelated
  accounts, then just create a config file for each account.

  [[./docs/accounts.png]]

* Downloads

  *ALPHA*

  This is *alpha* software.  It will not damage any running EC2
  Instances on your Amazon account.  It will only modify your Reserved
  Instances.  Modifying Reserved Instances does not terminate (or
  alter in any way) your running EC2 Instances.

  *You should run the application with --dry-run to see what it would
  attempt to do before actually running it without.*

  In all cases you should monitor your account(s) to make sure your
  Reserved Instances match your running instances during our alpha &
  beta periods.

*** [[http://place-where-we-download.com][Docker Image]]

    Hosted on DockerHub.

*** [[http://place-where-we-download.com][Ubuntu Linux (64) Binary]]

    Requires that libgmp (apt) is installed.  We will create a PPA
    with packages as time permits.

*** [[http://place-where-we-download.com][Apple OS X (64) Binary]]

    Requires that libgmp (Homebrew) is installed.

*** [[http://place-where-we-download.com][Windows 7/8 (64) Binary]]

    Requires that [[https://github.com/fpco/minghc][minghc]] is installed.  Later we could trim this down
    to just libgmp. _I'm not a Windows expert though. Pull requests to
    the Build section of this document are appreciated._

* Running

  #+begin_src sh
    rifactor --help
  #+end_src

*** Permissions

    Create a new IAM User on each of your AWS accounts.  This user
    account will be used to access your amazon account & modify
    Reserved Instances.

    Save the credentials (access key & secret key) given to you when
    you create the new user.  The name of the IAM User does not
    matter.  We only need the keys.

    Now add a User Policy to your IAM User that allows describing EC2
    resources & modifying EC2 Reserved Instances.

    #+begin_src js
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
    #+end_src

*** Config File

    On your local filesystem. Create a JSON file with the details of
    your accounts on AWS. Place the IAM access key & secret key from
    each user into the config file.
    #+begin_src js
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
    #+end_src

  The exact format of "regions" is located [[https://github.com/brendanhay/amazonka/blob/master/core/src/Network/AWS/Types.hs#L412][here]]. I've also listed it
  below.  This should only change when Amazon adds new regions.

  | Region          | Location                                               |
  |-----------------+--------------------------------------------------------|
  | Ireland         | Europe / eu-west-1                                     |
  | Frankfurt       | Europe / eu-central-1                                  |
  | Tokyo           | Asia Pacific / ap-northeast-1                          |
  | Singapore       | Asia Pacific / ap-southeast-1                          |
  | Sydney          | Asia Pacific / ap-southeast-2                          |
  | Beijing         | China / cn-north-1                                     |
  | NorthVirginia   | US / us-east-1                                         |
  | NorthCalifornia | US / us-west-1                                         |
  | Oregon          | US / us-west-2                                         |
  | GovCloud        | AWS GovCloud / us-gov-west-1                           |
  | GovCloudFIPS    | AWS GovCloud (FIPS 140-2) S3 Only / fips-us-gov-west-1 |
  | SaoPaulo        | South America / sa-east-1                              |

* Building

  You need the GHC 7.8.x compiler & cabal-install ([[http://brew.sh/][Homebrew]], [[https://github.com/fpco/minghc][minghc]] or
  [[https://launchpad.net/~hvr/%2Barchive/ubuntu/ghc][Ubuntu PPA]] will work). Review the [[.travis.yml][Travis CI Config File]] for build
  steps.

* Contributing

  Create a fork & submit a [[../pulls][pull request]] if you would like.  Github
  [[../issues][issues]] is the place to file your desires and grievances.
