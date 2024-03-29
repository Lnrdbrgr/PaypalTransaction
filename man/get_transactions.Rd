% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_calls.R
\name{get_transactions}
\alias{get_transactions}
\title{Request Historic Transactions}
\usage{
get_transactions(access_token, start_date, end_date, page = 1, page_size = 500)
}
\arguments{
\item{access_token}{The access token, retrieved by function request_access_token.}

\item{start_date}{Date from which transactions are to be shown in format YYYY-MM-DD.
Transactions can be accessed for a span of 30 days.}

\item{end_date}{Date until which transactions are to be shown in format YYYY-MM-DD.
Transactions can be accessed for a span of 30 days.}

\item{page}{Page to be returned. Default set to 1, if more than one page is needed
for all transactions a warning is printed}

\item{page_size}{Transactions to be displayed on each page. Default is set to maximum
value of 500.}
}
\value{
The list of transactions.
}
\description{
This function retrieves the historic transactions given
the access token and a time span.
}
