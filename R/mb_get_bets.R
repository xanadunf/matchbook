#' Get List of Current Bets on Matchbook
#' @name mb_get_bets
#' @description List the first 500 bets that have been made on Matchbook events that have not yet settled.
#' @param session_data A session object returned from a successful mb_login attempt. It contains details about your user preferences and security details.
#' @param event_ids A vector of event_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all events unless market_ids or runner_ids are specified.
#' @param market_ids A vector of market_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all markets unless event_ids or runner_ids are specified.
#' @param runner_ids A vector of runner_ids for which a list of current bets is required. This is an optional parameter and the default is to return bets from all runners unless event_ids or market_ids are specified.
#' @param sides A filter to allow selection of either 'back' or 'lay' bets. The default is to return 'back' bets.
#' @param status The bet status from one of the possible options ('matched','unmatched','cancelled','expired','opened','paused'). By default 'matched' bets are returned.
#' @param interval Time filter (in seconds) to allow selection of bets that were created or updated in the period between the currnet time and the current time minus the specified number of seconds. 
#' @param language A string with the language parameter e.g. 'en'
#' @return If successful, a dataframe with first 500 bets and associated information. Only 500 bets are permitted at one time. Pagination is possible but not implemented in this version.
#' @seealso \code{\link{mb_get_sports},\link{mb_get_events},\link{mb_get_markets}}
#' @export 
#' @examples
#' \dontrun{my_session <- mb_login("my_user_name","my_password"); mb_get_bets(session_data=my_session)}
#' 

mb_get_bets <- function(session_data,event_ids=NULL,market_ids=NULL,runner_ids=NULL,sides = c("back"),status=c("matched"),interval=0,language="en")
{
  #todo: runner states, include-prices, price-depth
  #event_ids=NULL;market_ids=NULL;runner_ids=NULL;sides = c("back");status=c("matched");interval=0;language="en";
  valid_sides        <- c("back","lay")
  valid_status       <- c("matched","unmatched","cancelled","expired","opened","paused")
  valid_languages    <- c("en","de","es","fr","hi","id","ja","ru","th","zh","zh_HANS","zh_HANT")
  
  content            <- NULL
  if(is.null(session_data)){
    print(paste("You have not provided data about your session in the session_data paramteter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }
  if(!is.null(event_ids)){
    if(event_ids%%1>0) print(paste("The event_ids must be integers. Please amend and try again."));return(content)
  }
  if(!is.null(market_ids)){
    if(market_ids%%1>0)  print(paste("The market_ids must be integers. Please amend and try again."));return(content)
  }
  if(!is.null(runner_ids)){
    if(runner_ids%%1>0) print(paste("The runner_ids must be integers. Please amend and try again."));return(content)
  }
  if(sum(!is.element(sides,valid_sides))>0){
    print(paste("All sides values must be one of",paste(valid_sides,collapse=","),". Please amend and try again."));return(content)
  }
  if(!is.element(language,valid_languages)){
    print(paste("The language parameter must be one of",paste(valid_languages,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.element(status,valid_status))>0){
    print(paste("All status values must be one of",paste(valid_status,collapse=","),". Please amend and try again."));return(content)
  }
  if(interval%%1>0){
    print(paste("The interval must be and integer. Please amend and try again."));return(content)
  }
  
  param_list         <- list('exchange-type'='back-lay','side'=paste(sides,collapse=","),'per-page'='500',language=language,status=paste(status,collapse=","))
  if(!is.null(event_ids)){
    param_list <- c(param_list,'event-ids'=paste(event_ids,collapse=","))
  }
  if(!is.null(market_ids)){
    param_list <- c(param_list,'market-ids'=paste(market_ids,collapse=","))
  }
  if(!is.null(runner_ids)){
    param_list <- c(param_list,'runner-ids'=paste(runner_ids,collapse=","))
  }
  if(interval>0){
    param_list <- c(param_list,interval=interval)
  }
  get_bets_resp    <- GET(paste("https://www.matchbook.com/bpapi/rest/offers",sep=""),query=param_list,set_cookies('session-token'=session_data$session_token),add_headers('User-Agent'='rlibnf'))  
  status_code        <- get_bets_resp$status_code  
  if(status_code==200)
  {
    content <- fromJSON(content(get_bets_resp, "text", "application/json"))$offers
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/offers",sep=""))
  }
  return(content)
}



