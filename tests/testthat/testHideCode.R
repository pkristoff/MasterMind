
context("MasterMind - hideCode")

source('../../R/server.R')

test_that("hideCode numOfPicks=1", {
  showCode <- mmServer('showCode')
  hideCode <- mmServer('hideCode')
  out <- list()
  result <- showCode(out, c('blue'), 1)
  result <- hideCode(result)
  expect_equal(result$codecell1(), '')
  expect_equal(result$codecell2(), '')
  expect_equal(result$codecell3(), '')
  expect_equal(result$codecell4(), '')

})

test_that("showCode numOfPicks=2", {
  showCode <- mmServer('showCode')
  hideCode <- mmServer('hideCode')
  out <- list()
  result <- showCode(out, c('red', 'blue'), 2)
  result <- hideCode(result)
  expect_equal(result$codecell1(), '')
  expect_equal(result$codecell2(), '')
  expect_equal(result$codecell3(), '')
  expect_equal(result$codecell4(), '')

})

test_that("showCode numOfPicks=3", {
  showCode <- mmServer('showCode')
  hideCode <- mmServer('hideCode')
  out <- list()
  result <- showCode(out, c('red', 'blue', 'orange'), 3)
  result <- hideCode(result)
  expect_equal(result$codecell1(), '')
  expect_equal(result$codecell2(), '')
  expect_equal(result$codecell3(), '')
  expect_equal(result$codecell4(), '')

})

test_that("showCode numOfPicks=3", {
  showCode <- mmServer('showCode')
  hideCode <- mmServer('hideCode')
  out <- list()
  result <- showCode(out, c('red', 'blue', 'orange', 'black'), 4)
  result <- hideCode(result)
  expect_equal(result$codecell1(), '')
  expect_equal(result$codecell2(), '')
  expect_equal(result$codecell3(), '')
  expect_equal(result$codecell4(), '')

})

