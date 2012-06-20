\name{irecord}
\alias{irecord}
\alias{ireplay}
\title{Record and replay iterators}
\description{
The \code{irecord} function records the values issued by a specified
iterator to a file or connection object.  The \code{ireplay} function
returns an iterator that will replay those values.  This is useful
for iterating concurrently over multiple, large matrices or data frames
that you can't keep in memory at the same time.  These large objects
can be recorded to files one at a time, and then be replayed
concurrently using minimal memory.
}
\usage{
irecord(con, iterable)
ireplay(con)
}
\arguments{
  \item{con}{A file path or open connection.}
  \item{iterable}{The iterable to record to the file.}
}
\examples{
suppressMessages(library(foreach))

m1 <- matrix(rnorm(70), 7, 10)
f1 <- tempfile()
irecord(f1, iter(m1, by='row', chunksize=3))

m2 <- matrix(1:50, 10, 5)
f2 <- tempfile()
irecord(f2, iter(m2, by='column', chunksize=3))

# Perform a simple out-of-core matrix multiply
p <- foreach(col=ireplay(f2), .combine='cbind') \%:\%
       foreach(row=ireplay(f1), .combine='rbind') \%do\% {
         row \%*\% col
       }

dimnames(p) <- NULL
print(p)
all.equal(p, m1 \%*\% m2)
unlink(c(f1, f2))
}
\keyword{utilities}
