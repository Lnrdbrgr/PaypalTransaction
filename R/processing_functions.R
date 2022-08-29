#' @title Converts Transaction List to Dataframe
#'
#' @description This function transforms the transaction list to a dataframe.
#'
#' @param transaction_list The list containing all transactions from function
#' get_transactions
#' @param return_EUR_values_only When set to TRUE, only EURO-values are considered.
#' Transactions in foreign currencies are usually converted to EURO-values in a separate
#' transaction, so no information is lost.
#'
#' @return Dataframe with transactions.
#' @export
#'
#' @import dplyr
#'
#' @examples
transaction_list_to_df <- function(transaction_list,
                                   return_EUR_values_only = TRUE){

  # check if transaction list is of class transaction_list from previous function
  if (class(transaction_list) != "transaction_list"){
    print("WARNING list not of correct class")
  }

  # create dataframe to store transactions
  n <- length(transaction_list$transaction_details)
  df <- data.frame(
    date = character(n),
    time = character(n),
    value = numeric(n),
    currency_code = character(n),
    issuer = character(n)
  )

  # loop through transactions and store the negative ones
  for (i in 1:nrow(df)){

    # extract value and check if suitable
    value <- transaction_list$transaction_details[[i]]$transaction_info$transaction_amount$value %>%
      as.numeric()

    # if negative transaction store
    if (value < 0){
      try({

        # extract date and currency attributes
        timestamp <- transaction_list$transaction_details[[i]]$transaction_info$transaction_initiation_date
        currency_code <- transaction_list$transaction_details[[i]]$transaction_info$transaction_amount$currency_code

        # get issuer
        issuer <- transaction_list$transaction_details[[i]]$payer_info$payer_name$alternate_full_name
        if (is.null(issuer)){
          issuer <- transaction_list$transaction_details[[i]]$cart_info$item_details[[1]]$item_name
        }

        # store in dataframe
        df$date[i] <- substr(timestamp, 1, 10)
        df$time[i] <- substr(timestamp, 12, 19)
        df$value[i] <- abs(value)
        df$currency_code[i] <- currency_code
        df$issuer[i] <- issuer
      }, silent = TRUE)

    }
  }

  # clean dataframe
  df <- df %>%
    filter(nchar(.data$date) != 0)

  if (return_EUR_values_only){
    df <- df %>%
      filter(.data$currency_code == "EUR")
  }

  # return dataframe
  return(df)
}
