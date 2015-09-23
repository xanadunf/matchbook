#' Get List of Available Markets for a given Event
#' @name List Available Markets for an Event
#' @description List the Markets Available on Matchbook.com for a given Event
#' @param event_id The event_id integer for which a list of associated markets is required.
#' @param sport_ids A vector of integer sport_ids that indicated sports for which event details are required. e.g. c(15,1) gives Soccer and Pro Football (NFL)
#' @param market_states A vector of string containing the market states to return. Defaults to 'open' or 'suspended' market types.
#' @param market_types A vector of strings containing the required market types. Valid market types are either 'multirunner' or 'binary'. Both are returned by default.
#' @param grading_types A vector of strings containing the required grading types. Valid grading types are 'asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins'. All are returned by default.
#' @param include_runners A boolean parameter indicating if the runner names and id should be returned or not. Defaults to FALSE.
#' @param include_prices A boolean parameter indicating if the runner prices should be returned or not. Defaults to FALSE.
#' @param language A string with the language parameter e.g. 'en'
#' @return If successful, a dataframe with first 500 markets and associated information. Only 500 markets are permitted at one time. Pagination is possible but not implemented here.
#' @seealso \code{\link{mb_get_sports},\link{mb_get_events}}
#' @export 
#' @examples
#' \dontrun{mb_get_markets(event_id=309912)}
#' 

mb_get_markets <- function(event_id,market_states = c("open","suspended"),market_types=c("multirunner","binary"),grading_types=c('asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins'),include_runners=FALSE,include_prices=FALSE,language="en")
{
  #todo: runner states, include-prices, price-depth
  #event_id <- 309912;market_states = c("open","suspended");market_types=c("multirunner","binary");grading_types=c('asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins');language="en";include_runners=TRUE;include_prices=TRUE
  valid_market_states<- c("suspended","open")
  valid_market_types <- c("multirunner","binary")
  valid_grading_types<- c('asian-handicap','high-score-wins','low-score-wins','point-spread','point-total','single-winner-wins')
  valid_languages    <- c("en","de","es","fr","hi","id","ja","ru","th","zh","zh_HANS","zh_HANT")
  
  content            <- NULL
  if(event_id%%1>0){
    print(paste("The event_id must be an integer. Please amend and try again."));return(content)
  }
  if(sum(!is.element(market_states,valid_market_states))>0){
    print(paste("All market_states values must be one of",paste(valid_market_states,collapse=","),". Please amend and try again."));return(content)
  }
  if(!is.element(language,valid_languages)){
    print(paste("The language parameter must be one of",paste(valid_languages,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.element(market_types,valid_market_types))>0){
    print(paste("All market_types values must be one of",paste(valid_market_types,collapse=","),". Please amend and try again."));return(content)
  }
  if(sum(!is.element(grading_types,valid_grading_types))>0){
    print(paste("All grading_types values must be one of",paste(valid_grading_types,collapse=","),". Please amend and try again."));return(content)
  }

  param_list         <- list('market-states'=paste(market_states,collapse=","),'per-page'='500',language=language,'grading-types'=paste(grading_types,collapse=","),'market-types'=paste(market_types,collapse=","))
  if(include_runners){
    param_list <- c(param_list,'include-runners'='true')
  }
  if(include_prices){
    param_list <- c(param_list,'include-prices'='true')
  }
    
  get_events_resp    <- GET(paste("https://www.matchbook.com/bpapi/rest/events/",event_id,"/markets",sep=""),query=param_list)  
  status_code        <- get_events_resp$status_code  
  if(status_code==200)
  {
    content <- fromJSON(content(get_events_resp, "text", "application/json"))$markets
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/events/",event_id,"/markets",sep=""))
  }
  return(content)
}