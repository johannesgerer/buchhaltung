# Buchhaltung  [![Build Status](https://travis-ci.org/johannesgerer/buchhaltung.svg?branch=master)](https://travis-ci.org/johannesgerer/buchhaltung)

> What advantages does he derive from the system of book-keeping by double entry! It is among the finest inventions of the human mind; every prudent master of a house should introduce it into his economy.
> -- Johann Wolfgang von Goethe

*Buchhaltung* (['bu&#720;&chi;ˌhaltʊŋ], German *book keeping*) helps you keep track of your finances on the commandline with minimal effort. It provides tools that help you in creating a complete ledger of all your bank and savings accounts', credit cards', and other transactions, in a text-based ledger format, that is readable by the [ledger CLI tool](http://www.ledger-cli.org/) and its many [derivatives](https://github.com/ledger/ledger/wiki/Ports).

* Fetch your bank transaction directly via FinTS/HBCI/OFXDirectConnect
* Import transactions from PayPal (can be customized to other formats)
* Semi-automatically match transactions to accounts using Bayesian classification
* Semi-automatic transaction entry with meaningful suggestions in keyboard speed mode
 * It is couples/room-mates aware: Create several transaction simultaniuously (see Shared Mode)

## Status & Aim

### May 2015
I am actively and successfully using this software for over 4 years and my ledger now contains more than 12.000 transactions accurately and continuously tracking the finances of my spouse and me including four checking and two savings accounts, one credit card, two paypal accounts, two cash wallets in EUR, bitcoin trading (both physical and on exchanges) and other currencies like USD, GPB used on trips.

Shortly before presenting about *Buchhaltung* at [Berlin Hack and Tell](http://www.meetup.com/berlin-hack-and-tell), I decided to give it to the open source community.

The software is pre-alpha and I am looking for early adopters and their use cases. The aim of this stage is to agree about the functionality and customizability and produce a first shipable version, that can be used without tinkering with the source.

Right now, I am using it on Linux but it should also run on Windows and Mac.

## Installation

### Prerequisites

* [Glasgow Haskell Compiler](https://www.haskell.org/) and [Cabal](https://www.haskell.org/cabal/)

  Required to **compile** the software. Packages available (ghc and cabal-install) e.g. on Ubuntu and ArchLinux (I personally use [ArchHaskell](https://wiki.archlinux.org/index.php/ArchHaskell)). They are also provided by the [Haskell Platform](https://www.haskell.org/platform/) available for Windows, Mac & Linux.


* [AqBanking Command Line Tool](http://www2.aquamaniac.de/sites/aqbanking/index.php)

  This is required for **direct retrieval of bank transactions** via FinTS/HBCI/EBICS (Germany) or OFXDirectConnect (USA, Canada, UK). Packages available e.g. on Ubuntu (aqbanking-tools) and ArchLinux (aqbanking). (AqBanking is also the used by [GnuCash](http://wiki.gnucash.org/wiki/AqBanking) for this purpose.)

* [dbacl](http://dbacl.sourceforge.net/)

  Bayesian classifier used to **match transaction to accounts**. Packages available e.g. on Ubuntu and ArchLinux ([AUR](https://aur.archlinux.org/packages/dbacl/)).

* [ledger CLI tool](http://www.ledger-cli.org/) or a compatible [port](https://github.com/ledger/ledger/wiki/Ports)

  ... to **query the ledger, create balance and report statements**, [web interface](http://hledger.org/manual.html#web), etc.

### Download, Compile & Install

```shell
# download
git clone https://github.com/johannesgerer/buchhaltung.git
cd buchhaltung


# compile
cabal update
cabal sandbox init 
cabal install

# create symlink to the binary
sudo ln -s "`pwd`/.cabal-sandbox/bin/buchhaltung" /usr/bin/buchhaltung

```

### Configure

Create a folder that will hold all your `config` and ledger files:

```shell
mkdir ~/.buchhaltung
cp -r sample-config.yml ~/.buchhaltung/config.yml
```

If you want a folder under a different location, either create a symlink or set the `BUCHHALTUNG` environment variable to that location.

#### The `~/.buchhaltung/config` file



## Usage

### First Usage / Clean

To initialize AqBanking after you edited the config file, you need to run:

```shell
buchhaltung user1 setup
```

To clean everythink aqbanking related remove the `cache` folder from your profile and then rerun the `setup` command.

### Import Transactions

#### 

```shell
buchhaltung user1 fetch
```

#### Resolve duplicates

Banks often minimally change the way they report transactions which leads to unwanted duplicates.

When importing, *Buchhaltung* will identify duplicates based on `([(Commodity,Quantity)], AccountName, Day)` and interactively resolve them by showing the user what fields have changed. If there are several candidates, it sorts the candidates according to similarity using `levenshteinDistance` from [edit-distance](https://hackage.haskell.org/package/edit-distance-0.2.1.3). (See [`Buchhaltung.Uniques.addNew`](https://github.com/johannesgerer/buchhaltung/blob/master/src/Buchhaltung/Uniques.hs#L27))
