# Matchbook.com API R package #

This R package provides some simple functions to enable interaction with the Matchbook.com RESTful API service.

* * *
# *Pre-requisites* 

* a valid Matchbook.com username and password
* R-version >= 3.0
* R-packages: httr, jsonlite

* * *
# Supported Functions

The following two functions aid with session Account verification

* **mb_login**
* **mb_logout**

The following functions provide the ability to get information on the sports that are available. This provides the ids that Matchbook uses for each sport. With these, you can obtain all of the events for the sports that you are interested in. Following on from that, you can obtain market ids and runner ids for any list of event ids. See the R documentation for more details on this.

* **mb_get_sports**
* **mb_get_events**
* **mb_get_markets**

I
The following function can be used to obtain details of any bets that have been placed but have not yet settled. Both details of unmatched and matched bets can be obtained. See the R documentation for more details (e.g. ? mb_get_bets)

* **mb_get_bets**

The following functions provide the core betting functionality.

* **mb_bet_place**
* **mb_bet_update**
* **mb_bet_cancel**

In order to obtain details on bets that have settled the following function can be used. Again, full function parameters can be see in the R documentation (?mb_get_settled)

* **mb_get_settled**

* * *
# Quickstart Tutorial
This should help you get up and running in a very short time, assuming that you have met the pre-requisites.
Lets set a target in this tutorial to place a bet on the first football team that contains the name 'man'.

To start, you need to login with your Matchbook.com account credentials.
``` 
library(matchbook)
username <- "my_user_name"
password <- "verysafepassword"
session_details <- mb_login(username,password)
```
 
Since we are betting on football, we need to find out the id of that sport.
``` 
sport_id_data <- mb_get_sports(session_details)
sport_id_data

 ```


mb_get_events(session_data=session_details,sport_ids=c(15,9))
mb_get_markets(session_data=session_details,event_id=311332)

## Get Prices

## Place a Bet

* * *
# Bug-Reporting

* * *
# Development Roadmap

* * *
# Disclaimer

