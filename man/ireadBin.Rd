\name{ireadBin}
\alias{ireadBin}
\title{Create an iterator to read binary data from a connection}
\description{
Create an iterator to read binary data from a connection.
}
\usage{
ireadBin(con, what, n=1L, \dots, ipos)
}
\arguments{
  \item{con}{A connection object or a character string naming a file or a
  raw vector.}
  \item{what}{Either an object whose mode will give the mode of the vector
  to be read, or a character vector of length one describing
  the mode: one of \dQuote{numeric}, \dQuote{double}, \dQuote{integer},
  \dQuote{int}, \dQuote{logical}, \dQuote{complex}, \dQuote{character},
  \dQuote{raw}.}
  \item{n}{integer.  The (maximal) number of records to be read each time
  the iterator is called.}
  \item{\dots}{Passed to \code{readBin} everytime the iterator is called.}
  \item{ipos}{Position iterable.  If specified, values from this iterable
  will be used to do a seek on the file before calling readBin.  This can
  be used to implement a distributed iterator.}
}
\examples{
zz <- file("testbin", "wb")
writeBin(1:100, zz)
close(zz)

it <- ihasNext(ireadBin("testbin", integer(), 10))
while (hasNext(it)) {
  print(nextElem(it))
}
unlink("testbin")
}
\keyword{utilities}
