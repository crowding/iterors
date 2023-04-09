# unifying ienumerate, isplitrows, isplitcols, chunk and chunkSize.????

# Template with shared logic used by icount, idiv, iteror.default
count_template <- function(input,
                           output,
                           preamble=list()) {
  count <- 0

  args <- as.pairlist(eval(bquote(
    splice=TRUE,
    alist(..(input), ...=, recycle=FALSE, chunkSize=1L, chunks=))))

  body <- bquote(splice=TRUE, {
    ..(preamble)

    if (!is.numeric(count) || length(count) != 1 || is.na(count))
      stop('count must be a numeric value')

    (function() NULL)(...) # any leftover arguments are an error

    i <- count
    i[1] <- 0L
    storage.mode(i) <- "integer"

    if (missing(chunks)) {
      if (chunkSize == 1L) { # single stepping
        if (is.finite(count)) {
          if (recycle) {
            nextOr_ <- function(or) { # recycling, non-chunking
              if (i >= count) {
                i[1] <<- 1
                .(output(i, 1))
              } else {
                i <<- i + 1L
                .(output(i, 1))
              }
            }
          } else {
            nextOr_ <- function(or) {  # non-recycling, non-chunking
              if (i >= count) {
                or
              } else {
                i <<- i + 1L
                .(output(i, 1))
              }
            }
          }
        } else {
          nextOr_ <- function(or) { # infinite, non-chunking
            i <<- i + 1L
            .(output(i, 1))
          }
        }
      } else { # chunking by chunkSize
        if (is.finite(count)) {
          last <- count - chunkSize
          nextOr_ <- function(or) {  # finite, chunking by chunkSize
            i.tmp <- i
            if (i >= last) {
              chunkSize <- count - i
              if (recycle) {
                i[1] <<- 0
              } else {
                i[1] <<- Inf
              }
              if (chunkSize < 1) return(or)
            } else {
              i[1] <<- i[1] + chunkSize
            }
            .(output(i.tmp, chunkSize))
          }
        } else {
          nextOr_ <- function(or) {  # infinite, chunking by chunkSize
            i.tmp <- i
            i[1] <<- i[1] + chunkSize
            .(output(i.tmp, chunkSize))
          }
        }
      }
    } else { # chunking by no. chunks
      chunks <- as.integer(chunks)
      chunksLeft <- chunks
      nextOr_ <- function(or) { # chunking by no. chunks
        repeat {
          if (chunksLeft <= 0L) {
            if (recycle) {
              chunksLeft <<- chunks
              i[1] <<- 0L
            } else {
              return(or)
            }
          }
          thisChunk <- as.integer(ceiling((count - i)/chunksLeft))
          chunksLeft <<- chunksLeft - 1L
          if (thisChunk==0) next
          i.tmp <- i
          i[1] <<- i[1] + thisChunk
          return(.(output(i.tmp, thisChunk)))
        }
      }
    }

    iteror.internal(nextOr_)
  })

  eval(call("function", args, body), parent.env(environment()))
}
