<!-- This buffer is for notes you don’t want to save.
  -- If you want to create a file, visit that file with C-x C-f,
  -- then enter the text in that file’s own buffer. -->

# Index of `iteror` functions

## methods
`hasNext`
`is.iteror`
`nextOr`
`ihasNext`

## Iterate over data held in memory
`iteror.array`
`iteror.default`
`iteror.data.frame`
`ienum`
`ienumerate`
`irepeat` # not to be confused with i_rep!

## Counting or mathematically defined iterators
`icombinations`
`icount`
`icountn`
`idiv`
`igrid`
`ipermutations`
`iseq`
`iseq_along`

## Random number generation
`irnbinom`
`irnorm`
`irpois`
`irunif`
`isample`
`irbinom`
`iRNGStream`
`iRNGSubStream`

## Iterator constructors based on files/functions
`iteror.function`
`itabulate`
`ireplay`
`iread.table`
`ireadBin`
`ireadLines`
`ireaddf`

## Consuming / summarizing an iteror
`as.vector` (`as.numeric`, `as.character`, ...)
`as.numeric`
`as.character`
`as.logical`
`as.list`
`nextOr`
`take`
`consume`
`count`
`record`
`nth`
`quantify`
`dotproduct`
`reduce`

## Higher order iteror functions (construct iterators in terms of other iterors)
`i_dotproduct`
`i_accum`
`i_apply`
`i_break`
`i_chain`
`i_chunk`
`i_collapse`
`i_compress`
`i_dedupe`
`i_drop`
`i_dropwhile`
`i_enumerate` (iemuerate.iteror...)
`i_keep`
`i_keepwhile`
`i_limit`
`i_map`
`i_pad`
`i_recycle`
`i_reduce`
`i_rep` (not to be confused with irepeat!)
`i_rle`
`i_rleinv`
`i_roundrobin`
`i_slice`
`i_star`
`i_starmap`
`i_tee`
`i_timeout`
`i_unique`
`i_window`
`i_zip`
`i_zip_longest`
