debuginfo <- function(what=NULL, debug = getOption('debug', F), fun.path = getOption('debug.add.path', F),
                      with.seps = getOption('debug.seps', T)) {
  ## * Print debug info if enabled with options(debug=T)
  objname <- paste(deparse(substitute(what)), collapse='\n')
  whatischar <- grepl('^"', objname)
  datetime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  ## print(sys.status())
  ## print(sys.nframe())
  ## print(str(sys.frames()))
  ## if (grepl('^str\\(', objname))
  ##   what <- eval(parse(text = paste0('capture.output(', deparse(sys.call(-length(sys.parents())+1)[[2]]), ')')))
  if (debug) {
    out <- capture.output((what))
    ## print(out)
    if (fun.path) {
      calling.funs <- paste0('In ', paste(sapply(seq_len(sys.nframe()-1), function(i) as.character(sys.calls()[[i]])[1]),
                            collapse='>'), ' : ')
    } else {
      if (sys.nframe() > 1) calling.funs <- paste0('In ', as.character(sys.calls()[[sys.nframe()-1]])[1], ' : ')
      else calling.funs <- ''
    }
    if (with.seps)  cat(paste(c(rep('_', getOption('width')),'\n'), collapse=''))
    if (length(out) == 1L) {
      ## print(glue::glue('\n{paste(rep("\t", sys.nframe()), collapse="")}>> In {calling.funs} : `{objname}` = "{what}".\n'))
      nf <- sys.nframe()
      if (!whatischar) {
        print(glue::glue('\n{nf} {datetime} >> {calling.funs}`{objname}` = "{what}".\n'))
      } else print(glue::glue('\n{nf} {datetime} >> {calling.funs}{what}.\n'))
    } else {
      
      ## print(glue::glue('\n{paste(rep("\t", sys.nframe()), collapse="")}>> In {calling.funs} : `{objname}` = \n'))
      print(glue::glue('\n{sys.nframe()} {datetime} >> {calling.funs}`{objname}` = \n'))
      if (last(capture.output(print(what))) == 'NULL') {
        cat(out[-length(out)], sep='\n')
      } else {
        print(what)
      }
    }
     if (with.seps)  cat(paste(c(rep('`', getOption('width')),'\n'), collapse=''))
  }
}

upper1st <- function(x, prelower = T) {
  if (is.factor(x)) {
    x0 <- levels(x)
  } else {
    x0 <- as.character(x)
  }
  if (prelower)
    x0 <- tolower(x0)
  nc <- nchar(x0)
  nc[is.na(nc)] <- 0
  x0[nc > 1] <- sprintf("%s%s", toupper(substr(x0[nc > 1], 1, 1)), substr(x0[nc > 1], 2, nchar(x0[nc > 1])))
  x0[nc == 1] <- toupper(x0[nc == 1])
  if (is.factor(x)) {
    levels(x) <- x0
  } else {
    x <- x0
  }
  return(x)
}
