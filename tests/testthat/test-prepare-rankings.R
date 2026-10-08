test_that("prepare_rankings converts participant rows to ranking columns", {
  input <- data.frame(
    ID = c("a", "b"), stat1 = c(-1, 0), stat2 = c(0, 1),
    stat3 = c(1, -1), check.names = FALSE
  )
  result <- prepare_rankings(input)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(3L, 2L))
  expect_equal(names(result), c("a", "b"))
  expect_equal(unname(result[[1L]]), c(-1, 0, 1))
})

test_that("prepare_rankings accepts a custom id_column", {
  input <- data.frame(
    participant = c("a", "b"), stat1 = c(-1, 0), stat2 = c(0, 1),
    stat3 = c(1, -1), check.names = FALSE
  )

  result <- prepare_rankings(input, id_column = "participant")

  expect_equal(names(result), c("a", "b"))
  expect_equal(row.names(result), c("stat1", "stat2", "stat3"))
})

test_that("prepare_rankings rejects invalid identifiers and values", {
  duplicated <- data.frame(ID = c("a", "a"), stat1 = 1:2, stat2 = 2:1,
                           stat3 = c(0, 0))
  expect_error(prepare_rankings(duplicated), "unique")

  nonnumeric <- data.frame(ID = "a", stat1 = 1, stat2 = "x", stat3 = 0)
  expect_error(prepare_rankings(nonnumeric), "numeric")
})
