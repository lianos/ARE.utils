###############################################################################
# GFF
###############################################################################

# The read/getAttribute code was extracted from davidTiling, and pasted
# back to the BioC mailing list (on 21-Oct-2008).
# 
# You can use it like so:
#     gff <- gff.read(gfffile)
#     gff$Name <- gff.getAttributeField(gff$attributes, "Name")
#     gff$ID <- gff.getAttributeField(gff$attributes, "ID")

gff.read <- function(gffFile, nrows=-1, verbose=FALSE) {
  cat("Reading:", gffFile, "\n")
  columns <- list(seqname='character', source='character', feature='character',
                  start='integer', end='integer', score='character', 
                  strand='character', frame='character', attributes='character')
  gff <- read.table(gffFile, sep="\t", as.is=TRUE, quote="",
                    header=FALSE, comment.char="#", nrows = nrows,
                    colClasses=unlist(columns))
  colnames(gff) <- names(columns)
  if (verbose) {
    cat("Found", nrow(gff), "rows with classes:", 
        paste(sapply(gff, class), collapse=", "), "\n")
  }
  if (any(is.na(gff$start)) | any(is.na(gff$end))) {
    stop("Missing start/stop values for some genes")
  }
  return(gff)
}

gff.getAttributeField <- function (x, field, attrsep = ";") {
  s <- strsplit(x, split = attrsep, fixed = TRUE)
  sapply(s, function(atts) {
    a <- strsplit(atts, split = "=", fixed = TRUE)
    m <- match(field, sapply(a, "[", 1))
    if (!is.na(m)) {
      rv <- a[[m]][2]
    } else {
      rv <- as.character(NA)
    }
    return(rv)
  })
}

gtf.read <- function(gffFile, nrows=-1, verbose=FALSE) {
  # type    : protein_coding, miRNA, pseudogene, snoRNA, ...
  # feature : CDS, exon, start_codon, stop_codon
  # strand  : +/-
  # Frame   : If the exon is in a coding region, this represents the reading
  #           frame of the first base (0-2), else '.'
  cat("Reading:", gffFile, "\n")
  columns <- list(chr='factor', type='factor', feature='factor',
                  start='integer', end='integer', score='factor', 
                  strand='factor', frame='factor', attributes='character')
  gff <- read.table(gffFile, sep="\t", as.is=TRUE, quote="",
                    header=FALSE, comment.char="#", nrows = nrows,
                    colClasses=unlist(columns))
  colnames(gff) <- names(columns)
  if (verbose) {
    cat("Found", nrow(gff), "rows with classes:", 
        paste(sapply(gff, class), collapse=", "), "\n")
  }
  if (any(is.na(gff$start)) | any(is.na(gff$end))) {
    stop("Missing start/stop values for some genes")
  }
  return(gff)
}

gtf.getAttributeField <- function (x, field, attrsep=";") {
  # Key/val pairs in attribute field are sep'd by 1 space
  # attributes are separated by a semicolon
  if (is.data.frame(x)) {
    x <- x$attributes
  }
  s <- strsplit(x, split=attrsep, fixed=TRUE)
  sapply(s, function(atts) {
    a <- unlist(strsplit(atts, split=" ", fixed=TRUE))
    m <- match(field, a)
    if (!is.na(m)) {
      val <- a[m+1]
      if (substr(val, 1, 1) == '"') {
        # Dequote the string if it's escaped
        val <- substr(val, 2, nchar(val)-1)
      }
    } else {
      val <- as.character(NA)
    }
    val
  })
}
