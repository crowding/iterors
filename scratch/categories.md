<!-- This buffer is for notes you don’t want to save.
  -- If you want to create a file, visit that file with C-x C-f,
  -- then enter the text in that file’s own buffer. -->

## do we really need?
iarray
isplit

## methods
hasNext
is.iteror
nextOr
ihasNext

## Self contained cloneable iterors:
icombinations
icount
icountn
idiv
ienum
ienumerate
igrid
ipermutations
irbinom
irepeat # not to be confused with irep!
iRNGStream
iRNGSubStream
irnbinom
irnorm
irpois
irunif
isample
itabulate # it refs a function, though...
iseq
iseq_along

### Iterator constructors based on files/functions
iteror.function
ireplay
iread.table
ireadBin
ireadLines
ireaddf

## Functions on iterors 
take
i_consume
i_length
i_record
i_nth
i_quantify

## Higher order iterors (constructed in terms of other iterors)
i_dotproduct
i_accum
i_apply
i_break
i_chain
i_chunk
i_collapse
i_compress
i_dedupe
i_drop
i_dropwhile
i_keep
i_keepwhile
i_limit
i_map
i_pad
i_recycle
i_reduce
i_rep (not to be confused with irepeat!)
i_rle
i_rleinv
i_roundrobin
i_slice
i_star
i_starmap
i_tee
i_timeout
i_unique
i_window
i_zip
i_zip_longest
