#!/bin/sh

LOGFILE=test.log

exec R --vanilla --slave > ${LOGFILE} 2>&1 <<'EOF'
suppressMessages(library(itertools))
suppressMessages(library(foreach))
suppressMessages(library(RUnit))

verbose <- as.logical(Sys.getenv('IT_VERBOSE', 'FALSE'))

options(warn=1)
options(showWarnCalls=TRUE)

cat('Starting test at', date(), '\n')

tests <- c('productTest.R', 'ihasNextTest.R', 'izipTest.R',
           'enumerateTest.R', 'ilimitTest.R', 'chainTest.R',
           'ifilterTest.R', 'irepTest.R', 'recycleTest.R',
           'ichunkTest.R')

errcase <- list()
for (f in tests) {
  cat('\nRunning test file:', f, '\n')
  t <- runTestFile(f)
  e <- getErrors(t)
  if (e$nErr + e$nFail > 0) {
    errcase <- c(errcase, t)
    print(t)
  }
}

if (length(errcase) == 0) {
  cat('*** Ran all tests successfully ***\n')
} else {
  cat('!!! Encountered', length(errcase), 'problems !!!\n')
  for (t in errcase) {
    print(t)
  }
}

cat('Finished test at', date(), '\n')
EOF
