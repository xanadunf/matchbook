# Matchbook.com API R package #

This R package provides some simple functions to enable interaction with the Matchbook.com RESTful API service.

* * *
# *Pre-requisites* 

* a valid Matchbook.com username and password
* R-version >= 3.0
* R-packages: httr, jsonlite

* * *
# Supported Functions

## Account verification
* mb_login
* mb_logout

## Get Sport/Event/Market/Runner Information
* mb_get_sports
* mb_get_events
* mb_get_markets

## Existing Bet Details
* mb_get_bets

## Bet Actions
* mb_bet_place
* mb_bet_update
* mb_bet_cancel

## Settled Bet Details
* mb_get_settled

* * *
# Quickstart Tutorial

## Login
``` 
library(matchbook)
username <- "my_user_name"
password <- "verysafepassword"
session_details <- mb_login(username,password)
 ```
## Get Event/Market/Runner Details
``` 
mb_get_sports(session_details)
mb_get_events(session_data=session_details,sport_ids=c(15,9))
mb_get_markets(session_data=session_details,event_id=311332)
 ```

## Get Prices

## Place a Bet

* * *
# Bug-Reporting

* * *
# Development Roadmap

* * *
# Disclaimer

