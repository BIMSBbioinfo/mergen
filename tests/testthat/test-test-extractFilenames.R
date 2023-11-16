test_argument_validation(
  function_name = "extractFilenames",
  argument_name = "text",
  argument_type = "string",
  allow_null = FALSE
)

test_that("No filename is returned when there is no filename", {
  prompt <- "There is no filename in here, just text and a little confusing code ```r xlsx=5```"
  expect_equal(extractFilenames(prompt),NA)
})


test_that("A filename is returned when there is an .xlsx filename", {
  prompt <- "There is a filename in here `my_file.xlsx`, also text and a little confusing code ```r xlsx=5```"
  expect_equal(extractFilenames(prompt),'my_file.xlsx')
})


test_that("A filename is returned when there is an .txt filename", {
  prompt <- "There is a filename in here `my_file.txt`, also text and a little confusing code ```r txt=5```"
  expect_equal(extractFilenames(prompt),'my_file.txt')
})

test_that("A filename is returned when there is an .csv filename", {
  prompt <- "There is a filename in here \n my_file.csv \n, also text and a little confusing code ```r csv=5```"
  expect_equal(extractFilenames(prompt),'my_file.csv')
})

test_that("A filename is returned when there is an .tsv filename", {
  prompt <- "There is a filename in here \n my_file.tsv \n, Also text and a little confusing code ```r tsv=5```"
  expect_equal(extractFilenames(prompt),'my_file.tsv')
})

test_that("A filename is returned when there is an .tsv filename", {
  prompt <- "There is a filename in here \n my_file.xls \n, Also text and a little confusing code ```r xls=5```"
  expect_equal(extractFilenames(prompt),'my_file.xls')
})


test_that("2 filenames are returned when there is an .tsv filename and a txt filename", {
  prompt <- "There is a filename in here \n my_file.xls \n my_other_file.txt. Also text and a little confusing code ```r xls=5```"
  expect_equal(extractFilenames(prompt),c('my_file.xls','my_other_file.txt'))
})

test_that("No filename is returned when there is a non-existing extension filename", {
  prompt <- "There is a filename in here \n my_file.weirdextension \n, Also text and a little confusing code ```r csv=5```"
  expect_equal(extractFilenames(prompt),NA)
})

test_that("No filename is returned when there is no real filename", {
  prompt <- "There is no filename in here. Xlsx is however used as a file extension. \n\n, Here is some text and a little confusing code ```r csv=5```"
  expect_equal(extractFilenames(prompt),NA)
})
