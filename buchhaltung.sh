#!/bin/bash

DIR="$(dirname "$(readlink -f "$0")")"
export PATH="$DIR"/.cabal-sandbox/bin/:$PATH

usage() {
				cat <<EOF

usage: ${0##*/} [user] [command] [file]
    
  Possible commands:
     - add            :: Enter new transactions
     - match          :: Match Transactoins to Ledger accounts (interactive)
     - fetch          :: fetches transactions and writes into ledger file
     - ok             :: record that something is ok into git
     - lb             :: list online-banking balances (from file)
     - j   [account]  :: ledger journal
     - b   [account]  :: bilanz
     - bv  [account]  :: bilanz mit aktuellen markpreisen
     - w              :: web-based version
     - paypal [file]  :: import paypal from CSV file  (alle guthaben relevanten 
                         Zahlungen (kommagetrennt) ohne warenkorbdetails!)
     - visa  [file]   :: import visa statements from comdiret CSV 
     - setup          :: initial setup
  
  User Profile folder:

      Default location: ~/.buchhaltung

      The folder location can overwritten by the BUCHHALTUNG
      environment variable.

  Config File: ~/.buchhaltung/config

  Plumbing
     - l         :: ledger
     - hl        :: hledger
     - req       :: Request transactions and balances (and save to file)
     - lt        :: List transactions in CSV (from file)
     - la        :: List accounts 
     - init      :: do req, if required
     - import    :: append transaction to ledger file
      -hbci      :: direct acces to aqhbcitool4
      -mkpinlist :: produce empty PIN file

EOF
}


conf="$BUCHHALTUNG/config"
if [ -z "$BUCHHALTUNG" -o ! -f "$conf" ]; then
		conf="$(readlink -f ~/.buchhaltung/config)"
		BUCHHALTUNG="$(dirname "$conf")"
fi

export BUCHHALTUNG="$BUCHHALTUNG"

if [ ! -f "$conf" ]; then echo "Error: Config file not found. $conf"; usage; exit 1; fi

#create exchange rate file
pricedb="$BUCHHALTUNG/exhange_rates.db"
touch "$pricedb"

USER="$1" # todo user

source "$conf"

#create cache for aqbanking
CACHE="$BUCHHALTUNG/cache"
mkdir "$CACHE"
aqhbcitool4="aqhbci-tool4 -C $CACHE/aqbanking-$USER"
aqbankingcli="aqbanking-cli -D $CACHE/aqbanking-$USER"
CONTEXT="$CACHE/context-$USER"

ABSLEDGERFOLDER="$BUCHHALTUNG/$LEDGERFOLDER"
BANKIMPORTLEDGER="$ABSLEDGERFOLDER/import.dat"
export LEDGER="$ABSLEDGERFOLDER/hledger.dat"
export FULLLEDGER="$ABSLEDGERFOLDER/ledger.dat"

mkdir -p "`dirname "$BANKIMPORTLEDGER"`"
[ ! -e "$BANKIMPORTLEDGER" ] && touch $BANKIMPORTLEDGER

IMPORT_TAG="EXSRC"

action () {
		
case "$1" in
		"match")
				(cd "$user" && buchhaltung-internal match  "$BANKIMPORTLEDGER" "$IMPORT_TAG")
				;;
		"fetch")
				rm "$CONTEXT" -f
				shift 
				if $aqbankingcli request  -c "$CONTEXT" --transactions $BLZS "$@"; then
						ff=`mktemp`
						action "lt" > "$ff"
						buchhaltung-internal aqbanking "$BANKIMPORTLEDGER" "$IMPORT_TAG" "$ff"
						rm $ff
						echo "Do you want to run 'Match' now?"
						select yn in "Yes" "No"; do
								case $yn in
										Yes )
												action "match"
												break;;
										No )
												break;;
								esac
						done
				else
						echo "AQbanking Error"
				fi
				;;
		"lb")
				$aqbankingcli listbal -c "$CONTEXT"
				;;
		"lt")
				$aqbankingcli listtrans -c "$CONTEXT" # --profile=default --profile-file=/home/data/finanzen/default.conf
				;;
		"hbci")
				shift
				$aqhbcitool4 "$@"
				;;
		"mkpinlist")
				shift
				$aqhbcitool4 mkpinlist "$@"
				;;
		"la")
				$aqhbcitool4 listaccounts #-u $ZUGANGSNUMMER
				;;
		"setup")
				$aqhbcitool4 adduser -t pintan --context=1 \
						-b $BLZ -u $ZUGANGSNUMMER \
						-s $URL \
						-N "$NAME" --hbciversion=$hbciv
				$aqhbcitool4 getsysid -u $ZUGANGSNUMMER
				$itanmode 
				$aqhbcitool4 getaccounts -u $ZUGANGSNUMMER
				# echo "noch \`del -i <UniqueId>' ausführen?"
				# action "del"
				;;
		"del")
				shift 
				echo "del -i <UniqueId>?"
				$aqhbcitool4 delaccount "$@"
				# account doppelt vorhanden --> umsätze doppelt
				;;
		"l")
				shift 
				export LEDGER="$FULLLEDGER"
				ledger --price-db "$pricedb" "$@"
				;;
		"visa")
				shift
				if [ ! -e "$origdir/$1" ]; then echo "ERROR file does not exist: $1"
				else
						buchhaltung-internal visa "$BANKIMPORTLEDGER" "$IMPORT_TAG" "$origdir/$1"
				fi
				;;
		"paypal")
				shift
				if [ ! -e "$origdir/$1" ]; then echo "ERROR file does not exist: $1"
				else
						ff=`mktemp`
						buchhaltung-internal paypal "$BANKIMPORTLEDGER" "$IMPORT_TAG" "$origdir/$1" 
				fi
				;;
		"add")  #FERTIG
				shift 
				(cd "$user" && buchhaltung-internal add "$BANKIMPORTLEDGER" "$user" "$@")
				;;
		"j")
				shift
				export LEDGER="$FULLLEDGER"
				sleep 2 | unbuffer -p ledger --price-db "$pricedb" reg --sort d "$@" | less -
				;;
		"w")
				shift
				hledger-web  "$@"
				;;
		"b")
				shift
				export LEDGER="$FULLLEDGER"
				sleep 2 | unbuffer -p ledger --price-db "$pricedb" bal  "$@" | less -
				;;
		"hl")
				shift 
				hledger "$@"
				;;
		"ok")
				shift
				git add -u "$user"
				export LEDGER="$FULLLEDGER"
				if [ "$user" = "Jo" ]; then
						git commit -F- <<EOF
OK: $@

Bank Balances:

`action "lb"`

Ledger Balacnes:

`ledger --price-db "$pricedb" bal -e tomorrow`

EOF
				else
						git commit -F- <<EOF
OK: $@

Bank Balances:

`action "lb"`

Ledger Balacnes:

`ledger --price-db "$pricedb" bal -e tomorrow`

EOF
				fi
				;;
		*)
				usage
				;;
esac
}

shift
cmd="$1"
shift
action "$cmd" "$@"
