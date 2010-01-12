\name{ireadBin}
\alias{ireadBin}
\title{Create an iterator to read binary data from a connection}
\description{
Create an iterator to read binary data from a connection.
}
\usage{
ireadBin(con, what='raw', n=1L, size=NA_integer_, signed=TRUE,
         endian=.Platform$endian, ipos=NULL)
}
\arguments{
  \item{con}{A connection object or a character string naming a file or a
  raw vector.}
  \item{what}{Either an object whose mode will give the mode of the vector
  to be read, or a character vector of length one describing
  the mode: one of \dQuote{numeric}, \dQuote{double}, \dQuote{integer},
  \dQuote{int}, \dQuote{logical}, \dQuote{complex}, \dQuote{character},
  \dQuote{raw}.  Unlike \code{readBin}, the default value is \dQuote{raw}.}
  \item{n}{integer.  The (maximal) number of records to be read each time
  the iterator is called.}
  \item{size}{integer.  The number of bytes per element in the byte stream.
  The default, \sQuote{NA_integer_}, uses the natural size.}
  \item{signed}{logical.  Only used for integers of sizes 1 and 2, when it
  determines if the quantity on file should be regarded as a signed or
  unsigned integer.}
  \item{endian}{The endian-ness ('\dQuote{big}' or '\dQuote{little}') of
  the target system for the file.  Using '\dQuote{swap}' will force swapping
  endian-ness.}
  \item{ipos}{iterable.  If not \code{NULL}, values from this iterable
  will be used to do a seek on the file before calling readBin.}
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
