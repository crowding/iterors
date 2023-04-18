total <- 0
run(for (elem in iter) total <- total + i)


total <- 0
tryCatch(
  repeat {
    total <- total + nextElem(iter)
  }, error = \(err) {
    if (conditionMessage(err) == "StopIteration") stop(err)
  })


hailstone <- gen(function(n) {
  repeat {
    yield(n)
    if (n %% 2 == 0)
      n <- n / 2
    else
      n <- n * 3 + 1
    if (n == 1) break
  }
})

