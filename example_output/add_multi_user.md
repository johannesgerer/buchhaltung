```shell
$ buchhaltung add -w alice
(Reading journal from
"~/.buchhaltung/jo/import.ledger"
"~/.buchhaltung/jo/entered_by_jo.ledger")
...

Hi jo!

Use readline keys to edit, use tab key to complete account names.

A code (in parentheses) may be entered following transaction dates.

A comment may be entered following descriptions or amounts.

3380 Transactions found

Starting new transaction...
Enter amount (zero for any transaction) [0]: $ 100
Date [2016-12-19]:
Title: Dinner
Enter source account: Assets:Wallet

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2016/12/19 Dinner

       Account        |  Amount  | Assertion
----------------------+----------+-----------+----------
0,jo -> Wallet:Assets | $ -100.0 |           |
----------------------+----------+-----------+----------
Open Balance          | $ 100.0  |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: n

Account [Assets:Wallet]: Expenses:Food

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2016/12/19 Dinner

       Account        |  Amount  | Assertion
----------------------+----------+-----------+----------
0,jo    Wallet:Assets | $ -100.0 |           |
1,jo -> Food:Expenses |          |           |
----------------------+----------+-----------+----------
Open Balance          | $ 100.0  |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: h


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2016/12/19 Dinner

       Account        |  Amount  | Assertion
----------------------+----------+-----------+----------
0,jo    Wallet:Assets | $ -100.0 |           |
1,jo -> Food:Expenses | $ 50     |           |
----------------------+----------+-----------+----------
Open Balance          | $ 50.0   |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: N

Account [Expenses:Food]:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2016/12/19 Dinner

        Account          |  Amount  | Assertion
-------------------------+----------+-----------+----------
0,jo    Wallet:Assets    | $ -100.0 |           |
1,jo    Food:Expenses    | $ 50     |           |
2,alice -> Food:Expenses |          |           |
-------------------------+----------+-----------+----------
Open Balance             | $ 50.0   |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: r


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2016/12/19 Dinner

        Account          |  Amount  | Assertion
-------------------------+----------+-----------+----------
0,jo    Wallet:Assets    | $ -100.0 |           |
1,jo    Food:Expenses    | $ 50     |           |
2,alice -> Food:Expenses | $ 50     |           |
-------------------------+----------+-----------+----------
Open Balance             | 0        |

[r]est    [j]next   [u]ser    [+]add   [t]itle   [?]Help     [s]ave       [p]%
[h]alf    [k]prev   [n/N]ew   [e]dit   [d]ate    [x]remove   [Q]discard   [v]AT
                              [m]iss                         [c]lear
your action: s

#######  jo:  Balanced Transaction   #######

2016/12/19 Dinner    ; Entered on "2016-12-19T19:01:00Z" by 'buchhaltung' user jo
    Wallet:Assets                             $ -100.0
    Food:Expenses                                 $ 50
    Accounts receivable:Friends:alice:jo          $ 50



#######  alice:  Balanced Transaction   #######

2016/12/19 Dinner    ; Entered on "2016-12-19T19:01:00Z" by 'buchhaltung' user jo
    Accounts receivable:Friends:jo:jo         $ -50
    Food:Expenses                              $ 50



Save? [y/N] y
New transaction created for 'jo'
New transaction created for 'alice'
```
