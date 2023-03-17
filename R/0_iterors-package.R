rng.state <- new.env()

.onLoad <- function(lib, pkg) {
  delayedAssign("stream", assign.env=rng.state,
                eval.env=environment(),
                value=iRNGStream(convseed(get(".Random.seed", envir=.GlobalEnv))))
}
