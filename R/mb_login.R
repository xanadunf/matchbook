#' Login and Authenticate with the Matchbook.com API
#' @name mb_login
#' @description Validate your session with the Matchbook API.
#' @param username Your Matchbook.com username. If you are not already registered go to \href{www.matchbook.com}{Matchbook.com}.
#' @param password The password set by you during the registration process.
#' @param print_balance_details optional parameter to allow balance and exposure display at the time of login
#' @return A list with status_code and other fields relating to your session and account. If the log-in is successful, a status_code of 200 is returned. 
#' @seealso \code{\link{mb_logout}}
#' @export 
#' @examples
#' \dontrun{mb_login("my_username","verysafepassword")}

mb_login <- function(username,password,print_balance_details=TRUE)
{
  content <- list(status_code=0)
  if(nchar(username)==0|nchar(password)==0){
    print("Invalid username or password format ...")
    return(content)  
  }else
  {
    session_token <-"";user_id<- 0;lnaguage="";odds_type<-"";currency<-"";user_name<-"";balance <- "not_available";exposure <- "not_available";
    body_data     <- paste("{'username': '",username,"', 'password': '",password,"'}",sep="")
    login_resp    <- httr::POST("https://www.matchbook.com/edge/rest/security/session", body=body_data,httr::content_type_json(),httr::accept_json(),httr::add_headers('User-Agent'='rlibnf'))
    status_code   <- login_resp$status_code
    if(status_code==200)
    {
      login_resp_content <- jsonlite::fromJSON(content(login_resp, "text", "application/json"))
      session_token      <- login_resp_content$`session-token`
      language           <- login_resp_content$`account`$`language`
      odds_type          <- login_resp_content$`account`$`odds-type`
      currency           <- login_resp_content$`account`$`currency`
      user_name          <- login_resp_content$`account`$`username`
      balance            <- login_resp_content$`account`$`username`
      balance            <- login_resp_content$`account`$`balance`
      exposure           <- login_resp_content$`account`$`exposure`
      if(print_balance_details){
        print(paste("Your current balance is ",balance,currency,". Your current exposure is ",round(exposure,4),currency,"",sep=""))        
      }
    }
    content <- list(status_code=status_code,session_token=session_token,user_id=user_id,language=language,odds_type=odds_type,currency=currency,user_name=user_name,balance=balance,exposure=exposure)
    return(content)    
  }
}


