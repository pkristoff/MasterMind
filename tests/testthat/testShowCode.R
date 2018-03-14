
context("MasterMind - showCode")

source('../../R/server.R')

test_that("showCode numOfPicks=1", {
  showCode <- mmServer('showCode')
  out <- list()
  result <- showCode(out, c('blue'), 1)
  expect_equal(result$codecell1(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$codecell2, NULL)
  expect_equal(result$codecell3, NULL)
  expect_equal(result$codecell4, NULL)

})

test_that("showCode numOfPicks=2", {
  showCode <- mmServer('showCode')
  out <- list()
  result <- showCode(out, c('red', 'blue'), 2)
  expect_equal(result$codecell1(), "<p style='color: red ;'>O</p>")
  expect_equal(result$codecell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$codecell3, NULL)
  expect_equal(result$codecell4, NULL)

})

test_that("showCode numOfPicks=3", {
  showCode <- mmServer('showCode')
  out <- list()
  result <- showCode(out, c('red', 'blue', 'orange'), 3)
  expect_equal(result$codecell1(), "<p style='color: red ;'>O</p>")
  expect_equal(result$codecell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$codecell3(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$codecell4, NULL)

})

test_that("showCode numOfPicks=3", {
  showCode <- mmServer('showCode')
  out <- list()
  result <- showCode(out, c('red', 'blue', 'orange', 'black'), 4)
  expect_equal(result$codecell1(), "<p style='color: red ;'>O</p>")
  expect_equal(result$codecell2(), "<p style='color: blue ;'>O</p>")
  expect_equal(result$codecell3(), "<p style='color: orange ;'>O</p>")
  expect_equal(result$codecell4(), "<p style='color: black ;'>O</p>")

})

