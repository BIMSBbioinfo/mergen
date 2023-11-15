test_that("Setting up an agent gives warning for non-existent agents and combos", {
  expect_warning(setupopenaiAgent("gpt-nonex",type="chat"))
  expect_warning(setupopenaiAgent("gpt-4",type="completion"))
  expect_warning(setupopenaiAgent("text-davinci-003",type="chat"))
  expect_equal(setupopenaiAgent("gpt-4",type="chat")$name,"openai")
})
