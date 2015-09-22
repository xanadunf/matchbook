#' Login and Authenticate with the Matchbook.com API
#' @name Login and Authentication
#' @description Validate your session with the Matchbook API.
#' @param username Your Matchbook.com username. If you are not already registered got to: https://www.matchbook.com/
#' @param password The password provided to you by email by the Matchbook.com team.
#' @return A list with status_code, user_id and session_token values for your session.
#' @seealso \code{\link{mb_logout}}
#' @export 
#' @examples
#' \dontrun{mb_login("my_username","verysafepassword")}

mb_login <- function(username,password)
{
  login_return <- NULL
  if(nchar(user_name)==0|nchar(password)==0){
    print("Invalid username or password format ...")
    return(login_return)  
  }else
  {
    session_token <-"";user_id<- 0
    body_data     <- paste("{'username': '",username,"', 'password': '",password,"'}",sep="")
    login_resp    <- POST("https://www.matchbook.com/bpapi/rest/security/session", body=body_data,content_type_json(),accept_json())
    status_code   <- login_resp$status_code
    if(status_code==200)
    {
      login_resp_content <- fromJSON(content(login_resp, "text", "application/json"))
      session_token      <- login_resp_content$`session-token`
      user_id            <- login_resp_content$`user-id`
    }
    login_return <- list(status_code=status_code,session_token=session_token,user_id=user_id)
    return(login_return)    
  }
}


