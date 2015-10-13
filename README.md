# R package for the Matchbook.com API service #

This R package provides some simple functions to enable interaction with the [Matchbook.com](http://www.matchbook.com) RESTful API service. Please read the documentation carefully for each individual function before use.  A registered [Matchbook.com](http://www.matchbook.com) account is required to use this package. Please read the documentation fully before use.

* * *
# *Pre-requisites & Installation* 

* a valid Matchbook.com username and password
* R-version >= 3.0
* R-packages: httr, jsonlite

To install the package requires the devtools library:
``` 
library(devtools)
install_github("xanadunf/matchbook")
``` 

* * *

# *Supported Functions*

The following two functions aid with account session verification

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

# *Quickstart Tutorial*
This should help you get up and running in a very short time, assuming that you have met the pre-requisites.
Let's set a target in this tutorial to place a bet on the first football game we find that contains the name 'man'. We will proceed to place a back bet of 5 EUR (or whatever the currency of your account) on the home team to win the game outright at odds of 1.10. 

To start, you need to login with your [Matchbook.com](http://www.matchbook.com) account credentials.
``` 
library(devtools)
install_github("xanadunf/matchbook")
library(matchbook)

username <- "my_user_name"     ### replace with your username
password <- "verysafepassword" ### replace with your password
session_details <- mb_login(username,password)
```
 Since we are betting on football, we need to find out the id of that sport.

```
sport_id_data <- mb_get_sports(session_details)
sport_id_data
football_sport_id <- sport_id_data$id[which(sport_id_data$name=="Soccer")]
football_sport_id
```

From the results, we can see that football has sport id=15. We can use this to get any football events with a team containing the 'man' string.

```
event_data <- mb_get_events(session_data=session_details,sport_ids=football_sport_id)
event_data[grep(tolower("man"), tolower(event_data$name)),]
test_event_id <- event_data$id[grep(tolower("man"), tolower(event_data$name))[1]]
test_event_id

```
Obviously, depending on when you run this, you may have many or no results. If you dont find a successful result, exchange 'man' for the name of a football team that you know is playing soon. 
So lets get all of the market data for this event. Since we want to bet on the match outcome, we will try to extract the 'single-winner-wins' market type.
```
market_data <- mb_get_markets(session_data=session_details,event_id=test_event_id,include_runners=TRUE)
market_data
test_market_id <- market_data$id[market_data$'grading-type'=="single-winner-wins"]
test_market_id
```
Now that we have the correct market, lets get information for all runners in this market. This time, we include the parameter 'include_prices=TRUE' so that we can see what price each runner is available at. Also, lets select the runner from the resulting data.
```
runner_data <- mb_get_runners(session_data=session_details,event_id=test_event_id,market_id=test_market_id,include_prices=TRUE)
runner_data
test_runner_id <- runner_data$id[grep("man", runner_data$name)]
test_runner_id
```
Now we have details on all of the runners, lets get details on the prices for the runner that we have selected.
```
prices_data <- runner_data$prices[[which(runner_data$id==test_runner_id)]]
prices_data_back <- prices_data[prices_data$side=="back",]
best_available_current_price <- min(prices_data_back$'decimal-odds') # min because we are backing, use the max if you are laying. 
```
Now that we have found the price levels that we can back at (the volume is also available via 'prices_data_back$'available-amount'') we can place a bet on the runner we have selected. 
```
mb_bet_place(session_data=session_details,runner_id=test_runner_id,side='back',stake=5,odds=1.10)
mb_get_bets(session_data=session_details)
```
When the bet is placed, its important to examine the status of the bet. If the status is 'matched' then you have been matched at the price provided. If it is 'open' then the bet has not been fully matched. For further details on placed bets check out the documentation: ?mb_bet_place
```
mb_get_bets(session_data=my_session,runner_id=test_runner_id)
```

* * *

# *Status*
This package is under active development and feedback and suggested improvements are welcome.

* * *

# *Bug-Reporting*
Please create an issue using the [Issues](https://github.com/xanadunf/matchbook/issues) area.

* * *

# *Future Development*
* add pagination to the results of calls where applicable.
* add parameter to allow sorting of prices returned.
* add function to retrieve account details
* add function to retrieve more detailed settlement report

* * *

# *Disclaimer*
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
