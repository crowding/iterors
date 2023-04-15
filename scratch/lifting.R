# so what I've been doing with count_template with N different
# functions depending on options is really a manually executed
# transform where you "lift" the IFs out of the function definition.

# for example:
nextOr <- LIFT_IF_(function(x) {
  if_(constant, action1, action2)
})

# becomes

if(constant)
  function(x) {
    action1
  }
else
  function(x) {
    action2
  }

# So my meandering nextOr definition in count_template would collapse to:

lift_if_(quote(nextOr <- function(or) {
  if_(missing(chunks), {
    if_(missing(chunkSize), {
      if_(is.finite(count),
          if (i >= count) {
            if_(recycle, i[1] <<- 0, return(or))
          })
      i <<- i + 1
      .(output(i))
    }, { # chunk by chunkSize
      if_(is.finite(count), {
        PREAMBLE(last <- count - chunkSize)
        i.tmp <- i
        if (i >= last) {
          chunkSize <- count - i
          if_(recycle,
              i[1] <<- 0,
              i[1] <<- Inf)
          if (chunkSize < 1) return(or)
            } else {
              i[1] <<- i[1] + chunkSize
            }
            .(output_chunk(i.tmp, chunkSize))
      }, { # chunk infinitely
        i[1] <<- i[1] + chunkSize
      }
      .(output_chunk(i.tmp, chunkSize))
      )
    })
  }, { # chunking by no. chunks
    PREAMBLE({
      ..(preamble_chunk)
      chunks <- as.integer(chunks)
      chunksLeft <- chunks
    })
    repeat {
      if (chunksLeft <= 0L) {
        if_(recycle,
        {
          chunksLeft <<- chunks
          i[1] <<- 0L
        },
        return(or))
      }
      thisChunk <- as.integer(ceiling((count - i)/chunksLeft))
      chunksLeft <<- chunksLeft - 1L
      if (thisChunk==0) next
      i.tmp <- i
      i[1] <<- i[1] + thisChunk
      return(.(output_chunk(i.tmp, thisChunk)))
    }
  })
}))

# Which, I'm not sure if that's easier to follow or not. I'm leaning not!
# But, the lifted if mechanism would also apply to templated
# arguments, like `rowMajor` or `simplify`
