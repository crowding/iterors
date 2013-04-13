\name{ireaddf}
\alias{ireaddf}
\title{Create an iterator to read data frames from files}
\description{
Create an iterator to read data frames from files.
}
\usage{
ireaddf(filenames, n, start=1, col.names, chunkSize=1000)
}
\arguments{
  \item{filenames}{Names of files contains column data.}
  \item{n}{Number of elements to read from each column file.}
  \item{start}{Element to starting reading from.}
  \item{col.names}{Names of the columns.}
  \item{chunkSize}{Number of rows to read at a time.}
}
\keyword{utilities}
