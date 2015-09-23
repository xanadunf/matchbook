#' Get List of Available Sports 
#' @name List Available Sports
#' @description List the Sports Available on Matchbook.com
#' @param status  A string with value one of 'active', 'pending'
#' @return If successful, the list of returned sports data.
#' @seealso \code{\link{mb_get_events}}
#' @export 
#' @examples
#' \dontrun{mb_get_sports()}
#' 

mb_get_sports <- function(status="active")
{
  content            <- NULL
  valid_states <- c("active","pending")
  if(!is.element(status,valid_states)){
    print(paste("Invalid status, it must a string with value one of",paste(valid_states,collapse=",")))
    return(content)
  }
  
  body_data     <- paste("{'status': '",status,"'}",sep="")
  get_sports_resp    <- GET("https://www.matchbook.com/bpapi/rest/lookups/sports",body=body_data)
  status_code        <- get_sports_resp$status_code
  if(status_code==200)
  {
    content <- fromJSON(content(get_sports_resp, "text", "application/json"))
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/lookups/sports",sep=""))
  }
  return(content)
}
