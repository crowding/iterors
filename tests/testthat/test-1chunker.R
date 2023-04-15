test_that("template iteror constructor", {

  ivector <- count_template(
    input = alist(obj=),
    preamble = alist(
      if (is.function(obj)) return(iteror.function(obj, ...)),
      count <- length(obj)),
    output = function(ix) substitute(obj[[ix]]), # unboxing!!!,
    output_chunk = function(ix, len) substitute(obj[ix + seq_len(len)])
  )

  expect_equal(as.numeric(ivector(1:10)), 1:10)
  expect_equal(c(recursive=TRUE, as.list(ivector(1:10, chunkSize=3))),
               1:10)
})
