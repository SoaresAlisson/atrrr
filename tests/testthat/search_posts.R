test_that("search for posts", {
  search_posts("rstats") |> expect_s3_class("tbl_df")
  search_posts("rstats", lang = "en")$record.langs |>
    unlist() |>
    unique() |>
    expect_equal("en")
})

test_that("search for posts - since until", {
  search_posts("rstats", since = "2024-01-01", until = "2024-08-01") |> expect_s3_class("tbl_df")
})
test_that("search for posts - limit", {
  search_bs <- search_posts("computational social sciences", limit = 100)

  search_bs |> expect_s3_class("tbl_df")
  expect_gte(nrow(search_bs), 25L)
})
