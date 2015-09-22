#' Logout of the Matchbook.com API session
#' @name Logout and Session termination
#' @description End the Matchbook.com session.
#' @param username Your Matchbook.com username. If you are not already registered got to: https://www.matchbook.com/
#' @param user_id Your Matchbook.com user_id obtained from the mb_login call. 
#' @param session_token Your Matchbook.com session_token obtained from the mb_login call. 
#' @return The response insteger status_code.
#' @seealso \code{\link{mb_login}}
#' @export 
#' @examples
#' \dontrun{mb_login("my_username","verysafepassword")}

mb_login <- function(username,password)
{
  login_return <- NULL
  if(nchar(username)==0|nchar(password)==0){
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


