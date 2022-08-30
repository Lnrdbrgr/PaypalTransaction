#' @title Request Access Token
#'
#' @description This function uses the credentials to request an access token
#' needed later in the transaction call. The token expires at some point.
#'
#' @param user_pwd The user credentials, consisting of a Client ID and Secret.
#' These should be concatenated as <Cient_ID:Secret>.
#' @param url Url to the paypal API, should not neccessarily be changed.
#' @param print_expiry_time TRUE prints the time of expiry for the token, default set to FALSE.
#' @param verbose TRUE prints response statements from the API, default set to FALSE.
#' @param return_token_only TRUE only returns the token, else a list with more attributes is returned,
#' default is set to FALSE.
#'
#' @return The access token only or a list with the access token and more attributes.
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import curl
#' @import rjson
#' @import httr
#' @import lubridate
#'
#' @examples
request_access_token <- function(user_pwd,
                                 url = "https://api.paypal.com/v1/oauth2/token",
                                 print_expiry_time = FALSE,
                                 verbose = FALSE,
                                 return_token_only = FALSE){

  # get token
  access_token <- curl::new_handle() %>%
    curl::handle_setopt(copypostfields = "grant_type=client_credentials",
                        userpwd = user_pwd,
                        verbose = verbose,
                        httpauth = 1L) %>%
    curl::handle_setheaders("Accept" = "application/json",
                            "Accept-Language" = "en_US") %>%
    curl::curl_fetch_memory(url = url) %$% content %>%
    rawToChar %>% rjson::fromJSON()

  # ToDo
  access_token$expires_on <- Sys.time() + lubridate::seconds(access_token$expires_in)
  if (print_expiry_time){
    print(paste0("Token expires on:  ", access_token$expires_on))
  }

  # return token
  if (return_token_only){
    return(access_token$access_token)
  } else {
    return(access_token)
  }

}




#' @title Request Historic Transactions
#'
#' @description This function retrieves the historic transactions given
#' the access token and a time span.
#'
#' @param access_token The access token, retrieved by function request_access_token.
#' @param start_date Date from which transactions are to be shown in format YYYY-MM-DD.
#' Transactions can be accessed for a span of 30 days.
#' @param end_date Date until which transactions are to be shown in format YYYY-MM-DD.
#' Transactions can be accessed for a span of 30 days.
#' @param page Page to be returned. Default set to 1, if more than one page is needed
#' for all transactions a warning is printed
#' @param page_size Transactions to be displayed on each page. Default is set to maximum
#' value of 500.
#'
#' @return The list of transactions.
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import curl
#' @import rjson
#' @import httr
#'
#' @examples
get_transactions <- function(access_token,
                             start_date,
                             end_date,
                             page = 1,
                             page_size = 500){

  # transform start and end date
  start_date = paste0(start_date, "T00:00:00-0700")
  end_date = paste0(end_date, "T23:59:59-0700")

  # construct the request
  request <- paste0("https://api.paypal.com",
                    "/v1/reporting/transactions?fields=all",
                    "&start_date=",
                    start_date,
                    "&end_date=",
                    end_date,
                    "&page=",
                    page,
                    "&page_size=",
                    page_size,
                    "&fields=all")

  # API request
  transaction_list <- new_handle() %>%
    curl::handle_setheaders("Accept" = "application/json",
                            "Authorization" = paste0("Bearer ", access_token)) %>%
    curl::curl_fetch_memory(url = request) %$% content %>%
    rawToChar %>% rjson::fromJSON()

  # print warning if more than one page needed
  if (!is.null(transaction_list$total_pages) && (transaction_list$total_pages > 1)){
    print(paste0("Page ", page, " of ", transaction_list$total_pages))
  }

  # return transaction list
  class(transaction_list) <- "transaction_list"
  return(transaction_list)

}




