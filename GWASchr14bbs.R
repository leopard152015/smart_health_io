
## Get all SYMBOLs on chr14
.getGWAS <- function(gene){
  symbols <- keys(Homo.sapiens, keytype="SYMBOL")
  map <- select(Homo.sapiens, symbols, "TXCHROM", "SYMBOL")
  symchoices <- sort(unique(map$SYMBOL[map$TXCHROM %in% "chr14"]))

## Possible BAM files
  bamchoices <- basename(RNAseqData.HNRNPC.bam.chr14_BAMFILES)

## where are the BAM files?
  dirname <- unique(dirname(RNAseqData.HNRNPC.bam.chr14_BAMFILES))

## What are the ranges of each gene?
  ranges <- genes(Homo.sapiens, columns="SYMBOL")
  ranges$SYMBOL <- unlist(ranges$SYMBOL)

## Create a representation of each gene region
  genes <- GeneRegionTrack(TxDb.Hsapiens.UCSC.hg19.knownGene,
                         chromosome="chr14", bamchoices[1])
  bam <- file.path(dirname,  bamchoices[1] )
  coverage <- Map(DataTrack,
                range = bam, name = bam,
                MoreArgs=list(type = 'histogram',
                              window = -1, genome = 'hg19',
                              chromosome = 'chr14',
                              transformation = function(x) asinh(x),
                              ylim=c(0, 8)))
  range <- ranges[match(gene, ranges$SYMBOL)]
  cg = range
  chr <- as.character(unique(seqnames(cg)))
  atrack <- AnnotationTrack(cg, name = "CpG") 
  itrack <- IdeogramTrack(genome = "hg19", chromosome = chr) 
  gtrack <- GenomeAxisTrack() 
## plot the GeneRegionTrack and coverage
  plotTracks(c(list(genes), coverage, itrack, gtrack, atrack),
           from = start(range), to=end(range),
           chr='chr14', windowSize = 30)
}

#plotTracks(list(itrack, gtrack, atrack))