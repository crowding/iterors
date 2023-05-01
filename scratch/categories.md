<!-- This buffer is for notes you don’t want to save.
  -- If you want to create a file, visit that file with C-x C-f,
  -- then enter the text in that file’s own buffer. -->

# Index of `iteror` functions by theme

## Basic methods
`hasNext`
`is.iteror`
`nextOr`
`ihasNext`

## Consuming / summarizing an iteror

###TODO: these should be s3 generic if they are not already.

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

By convention, in this package functions that _construct_ an iterator have names beginning with "`i`".

## Counting; mathematically defined iterators

`icombinations`
`icount`
`icountn`
`idiv`
`igrid`
`ipermutations`
`iseq`
`iseq_along`

## Iterate over data in memory

`iteror.array`
`iteror.default`
`iteror.data.frame`
`ienum`
`ienumerate` # !
`irepeat` # not to be confused with i_rep!

## Random number generation
`irnbinom`
`irnorm`
`irpois`
`irunif`
`isample`
`irbinom`
`iRNGStream`
`iRNGSubStream`

## Iterators based on files/functions

`itabulate` (???)
`ireplay`
`iread.table`
`ireadBin`
`ireadLines`
`ireaddf`

## Create an iterator with custom logic

`iteror.function`

## Higher order iteror functions (construct iterators in terms of other iterors)

By convention in this package, functions that transform iterors begin with `i_` with an underscore.

### Operating on data

`i_dotproduct`
`i_accum`
`i_apply`
`i_enumerate` (iemuerate.iteror...)
`i_map`
`i_reduce`
`i_star`
`i_starmap`


### Selection/filtering/limiting

`i_keep`
`i_keepwhile`
`i_limit`
`i_break`
`i_compress`
`i_dedupe`
`i_drop`
`i_dropwhile`
`i_unique`
`i_timeout`

### Sequencing (selecting, repeating)

`i_rep` (not to be confused with irepeat!)
`i_rle`
`i_rleinv`
`i_slice`
`i_pad`
`i_recycle`
`i_window`

### Splitting and combining

`i_chain`
`i_collapse`
`i_chunk`
`i_tee`
`i_zip`
`i_zip_longest`
`i_roundrobin`

