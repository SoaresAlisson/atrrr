#' search for posts
#'
#' Find posts matching search criteria, returning views of those posts
#'
#' @param query The term to be searched
#' @param tag The hashtag
#' @param since The start time
#' @param until The end time
#' @param lang The language
#' @param limit The number of results
#' @returns a nested tibble/data frame of posts
#' @export
#'
#' @examples
#' \dontrun{
#' search_posts("rstats")
#' search_posts("computational social sciences", limit = 100)
#' }
search_posts <- function(query,
                         tag = "",
                         since = "",
                         until = "",
                         lang = "",
                         limit = 25L) {
  res <- httr2::request("https://public.api.bsky.app/xrpc/app.bsky.feed.searchPosts") |>
    httr2::req_url_query(
      q = query,
      since = since,
      until = until,
      lang = lang,
      tag = tag,
      limit = limit
    ) |>
    httr2::req_headers(
      Accept = "application/json",
      Authorization = "Bearer <TOKEN>"
    ) |>
    httr2::req_perform()

  DF <- res$body |>
    rawToChar() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble()

  DF2 <- DF$posts |> tibble::as_tibble()

  return(DF2)
}
