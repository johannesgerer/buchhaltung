```
(Reading journal from
"/home/johannes/.buchhaltung/jo/import.ledger"
"/home/johannes/.buchhaltung/jo/entered_by_jo.ledger")
...

Hi jo!

Use readline keys to edit, use tab key to complete account names.

A code (in parentheses) may be entered following transaction dates.

A comment may be entered following descriptions or amounts.

3378 Transactions found

Starting new transaction...
Enter amount (zero for any transaction) [0]: 22.94
1)
2015/01/19 DANKE, IHR LIDL LASTSCHRIFT/BELAST.EC 60128709 170115204807OC1Ref. 1CA15019A2006931/20702    ; generated: by Common.fillTxn 2015-03-31 03:29:48.644423 CEST
    Assets:Accounts:BankA:Checking   -22.940 EUR
    Accounts payable:Lidl         22.940 EUR    ; HBCI: "";"20041133";"471681700";"";"";"2015/01/19";"2015/01/19";"-22.94";"EUR";"Johannes Gerer";"DANKE, IHR LIDL";"";"LASTSCHRIFT/BELAST.EC 60128709 170115204807OC1Ref. 1CA15019A2006931/20702";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";""


{number}: use one of the above 1 transactions
'm': enter transaction manually
'r': enter new amount to find existing transations
1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2015/01/19 DANKE, IHR LIDL LASTSCHRIFT/BELAST.EC 60128709 170115204807OC1Ref. 1CA15019A2006931/20702

                   Account                    |   Amount    | Assertion
----------------------------------------------+-------------+-----------+----------
0,jo    Accounts payable:Lidl                 | -22.940 EUR |           |
1,jo    Expenses:Food                         |             |           | 129
2,jo -> Account receivable:Friends:Marc       |             |           | 95
3,jo    Assets:Food storage                   |             |           | 5
4,jo    Expenses:Household                    |             |           | 1
----------------------------------------------+-------------+-----------+----------
Open Balance                                  | 22.940 EUR  |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: r


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2015/01/19 DANKE, IHR LIDL LASTSCHRIFT/BELAST.EC 60128709 170115204807OC1Ref. 1CA15019A2006931/20702

                   Account                    |   Amount    | Assertion | Frequency
----------------------------------------------+-------------+-----------+----------
0,jo    Accounts payable:Lidl                 | -22.940 EUR |           |
1,jo    Expenses:Food                         | 22.94 EUR   |           | 129
2,jo -> Account receivable:Friends:Marc       |             |           | 95
3,jo    Assets:Food storage                   |             |           | 5
4,jo    Expenses:Household                    |             |           | 1
----------------------------------------------+-------------+-----------+----------
Open Balance                                  | 0           |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: s

#######  jo:  Balanced Transaction   #######

2015/01/19 DANKE, IHR LIDL LASTSCHRIFT/BELAST.EC 60128709 170115204807OC1Ref. 1CA15019A2006931/20702    ; Entered on "2016-12-20T16:57:00Z" by 'buchhaltung' user jo
    Accounts payable:Lidl             -22.940 EUR
    Expenses:Food                      22.94 EUR



Save? [y/N] y
New transaction created for 'jo'

1 Transactions were changed

```
