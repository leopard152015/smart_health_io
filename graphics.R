

.ggplotParameter <- function (data,param, name) {
  
  param_split=split(param,as.factor(Subject.type))
  data$Subject.type <- factor(data$Subject.type,labels= c("Non-obese","Obese"))
 
 
 
  ggplot2.boxplot(data = data,formula= param_split, yName=name,
                  xName='Subject.type',
                  notch=TRUE,color="grey30",fill="grey30",
                  addDot=TRUE, dotSize=0.3, 
                  mainTitle=" ",
                  mainTitleFont=c(16,"bold.italic", "red"),
                  ytitle=name, xtitle="Sybject type",
                  xtitleFont=c(14,"bold", "#993333"), 
                  ytitleFont=c(14,"bold", "#993333"),
                  
                  groupName='Subject.type',
                  groupColors=c("cornflowerblue","brown3"),
                  orientation="horizontal")
}

.tableForm <- function(data){
  dt <- DT::datatable(data.frame(data),
                    #  options = list(scrollY = 350),    
                escape = FALSE,
                class = c('cell-border'), options = list(  
                pageLength = 9,  pageWidth = 15, dom = 'tip', autoWidth = F,
                autoWidth = FALSE,
                columnDefs = list(list(
                targets  = "_all",
                className = 'dt-center')) 
                )) 
  return(dt)              
}
# The Layered-Violin Graph

df.Clean <- function(df,Sep="."){
  nms   <- colnames(df)
  rws   <- rownames(df)
  
  # Change punctuation and blankss in variable names to points
  nmsP  <- gsub("([[:punct:]]|[[:blank:]])+","+",nms)
  nmsPP <- gsub("(^[+]|[+]$)+","",nmsP)
  nmsPP <- gsub("[+]",Sep,nmsPP)
  # Check for double names
  ifelse(length(unique(nmsPP))==length(nmsPP),{nms <- nmsPP},{
    id2 <- which(plyr::laply(nmsPP,function(n) sum(nmsPP%in%n))>1)
    nms <- nmsPP
    nms[id2] <- paste(nmsPP[id2],id2,sep=".")})
  
  colnames(df) <- nms
  df      <- dplyr::select(df,which(nms%in%nms[nms!=""]))
  df[ ,1] <- paste0("Row.",seq(1,nrow(df)))
  colnames(df)[1] <- paste("Local","ID",sep=Sep)
  return(list(df=df,
              nms=nms,
              rws=rws))
}

get.OSFfile <- function(# Function to download OSF file modified from code by Sacha Epskamp
  code,  #Either "https://osf.io/XXXXX/" or just the code
  dir = tempdir(), # Output location
  scanMethod, #  "readLines" or "RCurl". Leave missing to automatically chose
  downloadMethod = c("httr","downloader","curl"), # First one is chosen
  dataFrame = TRUE,
  sep = ',',
  dfCln = FALSE
){
  # Check if input is code:
  if (!grepl("osf\\.io",code)){
    URL <- sprintf("https://osf.io/%s/",code)
  } else URL <- code
  
  # Scan page:
  if (grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE)){
    try(setInternet2(TRUE))
  }
  
  if (missing(scanMethod)){
    scanMethod <- ifelse(grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE), "readLines", "RCurl")
  }
  if (scanMethod == "readLines"){
    Page <- paste(readLines(URL),collapse="\n")
  } else if (scanMethod == "RCurl"){
    library("RCurl")
    Page <- RCurl::getURL(URL)
  } else if (scanMethod == "httr"){
    Page <- httr::GET(URL)
    Page <- paste(Page,collapse="\n")
  } else stop("Invalid scanMethod")
  
  
  # Create download link:
  URL <- gsub("/$","",URL)
  #   Link <- paste0(URL,"/?action=download&version=1")
  Link <- paste0(URL,"/?action=download")
  
  # Extract file name:
  FileName <- regmatches(Page,gregexpr("(?<=\\OSF \\| ).*?(?=\\)", Page, perl=TRUE))[[1]]
  FullPath <- paste0(dir,"/",FileName)
  
  info <- NULL
  # Download file:
  if (downloadMethod[[1]]=="httr"){
    library("httr")
    info <- httr::GET(Link, httr::write_disk(FullPath, overwrite = TRUE))
  } else if (downloadMethod[[1]]=="downloader"){
    library("downloader")
    downloader:::download(Link, destfile = FullPath, quiet=TRUE)
  } else if (downloadMethod[[1]]=="curl"){
    system(sprintf("curl -J -L %s > %s", Link, FullPath), ignore.stderr = TRUE)
  }  else stop("invalid downloadMethod")
  
  df <- NULL
  if(dataFrame==TRUE){
    if(grepl('xls',FileName)){
      df <- tbl_df(read.xlsx2(file=FullPath,sheetIndex=1))
    } else {
      df <- tbl_df(read.table(FullPath,stringsAsFactors=F,fill = T,header=T,sep=sep, comment.char = "",quote = "\""))
    }
    if(dfCln==TRUE){df <- df.Clean(df)} else {df$df <- df}
    
    return(list(df   = df$df,
                info = list(FilePath=FullPath,
                            Info=info,
                            ori.Colnames=tbl_df(data.frame(ori.colnames=df$nms)),
                            ori.Rownames=tbl_df(data.frame(ori.rownames=df$rws))
                )))
  } else {
    # Return location of file:
    return(FilePath=FullPath)
  }
}


## PLOTS

gg.theme <- function(type=c("clean","noax")[1],useArial = F, afmPATH="~/Dropbox"){
  require(ggplot2)
  if(useArial){
    set.Arial(afmPATH)
    bf_font="Arial"
  } else {bf_font="Helvetica"}
  
  switch(type,
         clean = theme_bw(base_size = 16, base_family=bf_font) +
           theme(axis.text.x     = element_text(size = 14),
                 axis.title.y    = element_text(vjust = +1.5),
                 panel.grid.major  = element_blank(),
                 panel.grid.minor  = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line  = element_line(colour = "black")),
         
         noax = theme(line = element_blank(),
                      text  = element_blank(),
                      title = element_blank(),
                      plot.background = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
  )
}

set.Arial <- function(afmPATH="~/Dropbox"){
  # Set up PDF device on MAC OSX to use Arial as a font in Graphs
  if(nchar(afmPATH>0)){
    if(file.exists(paste0(afmPATH,"/Arial.afm"))){
      Arial <- Type1Font("Arial",
                         c(paste(afmPATH,"/Arial.afm",sep=""),
                           paste(afmPATH,"/Arial Bold.afm",sep=""),
                           paste(afmPATH,"/Arial Italic.afm",sep=""),
                           paste(afmPATH,"/Arial Bold Italic.afm",sep="")))
      if(!"Arial" %in% names(pdfFonts())){pdfFonts(Arial=Arial)}
      if(!"Arial" %in% names(postscriptFonts())){postscriptFonts(Arial=Arial)}
      return()
    } else {disp(header='useArial=TRUE',message='The directory did not contain the *.afm version of the Arial font family')}
  } else {disp(header='useArial=TRUE',message='Please provide the path to the *.afm version of the Arial font family')}
}

#function to create geom_ploygon calls
fill_viol<-function(gr.df,gr,qtile,probs){
  # SETUP VIOLIN QUANTILE PLOTS -----------------------------------
  # This is adapted from: http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot
  
  ifelse(is.null(qtile),{
    cuts <- cut(gr.df$y, breaks = quantile(gr.df$y, probs, na.rm=T, type=3, include.lowest = T, right = T), na.rm=T)},{
      cuts <- cut(gr.df$y, breaks = qtile, na.rm=T)
    }
  )
  quants <- mutate(gr.df,
                   x.l=x-violinwidth/2,
                   x.r=x+violinwidth/2,
                   cuts=cuts)
  
  plotquants <- data.frame(x=c(quants$x.l,rev(quants$x.r)),
                           y=c(quants$y,rev(quants$y)),
                           id=c(quants$cuts,rev(quants$cuts)))
  
  #cut by quantile to create polygon id
  geom <- geom_polygon(aes(x=x,y=y,fill=factor(id)),data=plotquants,alpha=1)
  
  return(list(quants=quants,plotquants=plotquants,geom=geom))
}

vioQtile <- function(gg=NULL,qtiles=NULL,probs=seq(0,1,.25),labels=paste(probs[-1]*100),withData=FALSE){
  require(ggplot2)
  # SETUP VIOLIN QUANTILE PLOTS -----------------------------------
  # This is adapted from: http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot
  #
  # Changed:
  # - Deal with 'empty' quantile groups
  # - Deal with original data
  # - More input, more output
  g.df <- ggplot_build(gg)$data[[1]]    # use ggbuild to get the outline co-ords
  
  ifelse(is.null(qtiles),{
    gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,NULL,probs)$geom)},{
      gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,qtiles[x, ],probs)$geom)}
  )
  
  gg <- gg + geom_hline(aes(yintercept=0)) +
    scale_fill_grey(name="Quantile\n",labels=labels,guide=guide_legend(reverse=T,label.position="right")) +
    stat_summary(fun.y=median, geom="point", size=8, color="grey80",shape=21,fill="white")
  
  if(withData){
    ifelse(is.null(qtiles),{
      ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,NULL,probs))},{
        ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,qtiles[x,],probs))
      }
    )
    return(list(ggGraph=gg,ggData=ggData))
  } else {
    return(gg)
  }
}

# MULTIPLOT FUNCTION ------------------------------------------------------------------------------------------------------------------
#
# [copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ]
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multi.PLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

.pValues <- function(p1,p2,p3){
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  
  yhigh <- 3
  #xi <- c(1, 0.5, 0.1)
  xi <- c(p1,p2,p3)
  plot(function(x) dbeta(x, xi[1], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 1, xlab = " ", ylab = " ")
  mtext("p value", 1, line = 2.5, cex = 1.5, font = 2)
  mtext("Density", 2, line = 3, cex = 1.5, font = 2, las = 0)

  par(new = TRUE)
  plot(function(x) dbeta(x, xi[2], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 2, xlab = " ", ylab = " ")

  par(new = TRUE)
  plot(function(x) dbeta(x, xi[3], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
     lwd = 2, lty = 3, xlab = " ", ylab = " ")

  cexsize <- 1.5
  text(0.5, 1.15, expression(xi == 1), cex = cexsize, pos = 4)
  text(0.1, 1.6, expression(xi == 0.5), cex = cexsize, pos = 4)
  text(0, 0.2, expression(xi == 0.1), cex = cexsize, pos = 4)
}
.plotMarginalCor <- function(variable, cexYlab = 1.3, lwd = 2, rugs = FALSE) {
  
  # histogram with density estimator
  
  variable <- variable[!is.na(variable)]
  
  density <- density(variable)
  h <- hist(variable, plot = FALSE)
  jitVar <- jitter(variable)
  yhigh <- max(max(h$density), max(density$y))
  ylow <- 0
  xticks <- pretty(c(variable, h$breaks), min.n = 3)
  plot(range(xticks), c(ylow, yhigh), type = "n", axes = FALSE, ylab = "", 
       xlab = "")
  h <- hist(variable, freq = FALSE, main = "", ylim = c(ylow, yhigh), xlab = "", 
            ylab = " ", axes = FALSE, col = "grey", add = TRUE, nbreaks = round(length(variable)/5))
  ax1 <- axis(1, line = 0.3, at = xticks, lab = xticks)
  par(las = 0)
  ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), 
                                                                      max(density$y))), labels = c("", "Density", ""), lwd.ticks = 0, 
              pos = range(ax1) - 0.08 * diff(range(ax1)), cex.axis = 0.4, mgp=c(0.1,0.1,0))#1.7, mgp = c(3, 
  #    0.7, 0))
  
  if (rugs) 
    rug(jitVar)
  
  lines(density$x[density$x >= min(ax1) & density$x <= max(ax1)], density$y[density$x >= 
                                                                              min(ax1) & density$x <= max(ax1)], lwd = lwd)
}

.poly.pred <- function(fit, line = FALSE, xMin, xMax, lwd) {
  
  # predictions of fitted model
  
  # create function formula
  f <- vector("character", 0)
  
  for (i in seq_along(coef(fit))) {
    
    if (i == 1) {
      
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep = "")
    }
    
    if (i > 1) {
      
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i - 1, sep = "")
      f <- paste(f, temp, sep = "+")
    }
  }
  
  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text = f))
  
  if (line == FALSE) {
    return(predY)
  }
  
  if (line) {
    lines(x, predY, lwd = lwd)
  }
}


.plotScatter <- function(xVar, yVar, cexPoints = 1.3, cexXAxis = 1.3, 
                         cexYAxis = 1.3, lwd = 2) {
  
  # displays scatterplot
  
  d <- data.frame(xx = xVar, yy = yVar)
  d <- na.omit(d)
  xVar <- d$xx
  yVar <- d$yy
  
  fit <- lm(yy ~ poly(xx, 1, raw = TRUE), d)
  
  xlow <- min((min(xVar) - 0.1 * min(xVar)), min(pretty(xVar)))
  xhigh <- max((max(xVar) + 0.1 * max(xVar)), max(pretty(xVar)))
  xticks <- pretty(c(xlow, xhigh))
  ylow <- min((min(yVar) - 0.1 * min(yVar)), min(pretty(yVar)), min(.poly.pred(fit, 
                                                                               line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)], 
                                                                               lwd = lwd)))
  yhigh <- max((max(yVar) + 0.1 * max(yVar)), max(pretty(yVar)), max(.poly.pred(fit, 
                                                                                line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)], 
                                                                                lwd = lwd)))
  
  
  yticks <- pretty(c(ylow, yhigh))
  
  yLabs <- vector("character", length(yticks))
  
  for (i in seq_along(yticks)) {
    
    if (yticks[i] < 10^6) {
      
      yLabs[i] <- format(yticks[i], digits = 3, scientific = FALSE)
      
    } else {
      
      yLabs[i] <- format(yticks[i], digits = 3, scientific = TRUE)
    }
  }
  
  plot(xVar, yVar, col = "black", pch = 21, bg = "grey", ylab = "", 
       xlab = "", axes = FALSE, ylim = range(yticks), xlim = range(xticks), 
       cex = cexPoints)
  .poly.pred(fit, line = TRUE, xMin = xticks[1], xMax = xticks[length(xticks)], 
             lwd = lwd)
  
  par(las = 1)
  
  axis(1, line = 0.4, labels = xticks, at = xticks, cex.axis =cexXAxis)
  axis(2, line = 0.2, labels = yLabs, at = yticks, cex.axis = cexYAxis)
  
  invisible(max(nchar(yLabs)))
}

.plotCorValue <- function(xVar, yVar, cexText = 2.5, cexCI = 1.7, hypothesis = "correlated", 
                          pearson = TRUE, kendallsTauB = FALSE, spearman = FALSE, confidenceInterval = 0.95) {
  
  # displays correlation value
  
  CIPossible <- TRUE
  
  tests <- c()
  
  if (pearson) 
    tests <- c(tests, "pearson")
  
  if (spearman) 
    tests <- c(tests, "spearman")
  
  if (kendallsTauB) 
    tests <- c(tests, "kendall")
  
  plot(1, 1, type = "n", axes = FALSE, ylab = "", xlab = "")
  
  lab <- vector("list")
  
  for (i in seq_along(tests)) {
    
    if (round(cor.test(xVar, yVar, method = tests[i])$estimate, 8) == 
        1) {
      
      CIPossible <- FALSE
      
      if (tests[i] == "pearson") {
        lab[[i]] <- bquote(italic(r) == "1.000")
      }
      
      if (tests[i] == "spearman") {
        lab[[i]] <- bquote(italic(rho) == "1.000")
      }
      
      if (tests[i] == "kendall") {
        lab[[i]] <- bquote(italic(tau) == "1.000")
      }
      
    } else if (round(cor.test(xVar, yVar, method = tests[i])$estimate, 
                     8) == -1) {
      
      CIPossible <- FALSE
      
      if (tests[i] == "pearson") {
        lab[[i]] <- bquote(italic(r) == "-1.000")
      }
      
      if (tests[i] == "spearman") {
        lab[[i]] <- bquote(italic(rho) == "-1.000")
      }
      
      if (tests[i] == "kendall") {
        lab[[i]] <- bquote(italic(tau) == "-1.000")
      }
      
    } else {
      
      if (tests[i] == "pearson") {
        lab[[i]] <- bquote(italic(r) == .(formatC(round(cor.test(xVar, 
                                                                 yVar, method = tests[i])$estimate, 3), format = "f", 
                                                  digits = 3)))
      }
      
      if (tests[i] == "spearman") {
        lab[[i]] <- bquote(rho == .(formatC(round(cor.test(xVar, 
                                                           yVar, method = tests[i])$estimate, 3), format = "f", 
                                            digits = 3)))
      }
      
      if (tests[i] == "kendall") {
        lab[[i]] <- bquote(tau == .(formatC(round(cor.test(xVar, 
                                                           yVar, method = tests[i])$estimate, 3), format = "f", 
                                            digits = 3)))
      }
    }
  }
  
  if (length(tests) == 1) {
    ypos <- 1
  }
  
  if (length(tests) == 2) {
    ypos <- c(1.1, 0.9)
  }
  
  if (length(tests) == 3) {
    ypos <- c(1.2, 1, 0.8)
  }
  
  
  for (i in seq_along(tests)) {
    
    text(1, ypos[i], labels = lab[[i]], cex = cexText)
  }
  
  
  if (hypothesis == "correlated" & length(tests) == 1 & any(tests == 
                                                            "pearson")) {
    
    alternative <- "two.sided"
    ctest <- cor.test(xVar, yVar, method = tests, conf.level = confidenceInterval)
  }
  
  if (hypothesis != "correlated" & length(tests) == 1 & any(tests == 
                                                            "pearson")) {
    
    if (hypothesis == "correlatedPositively") {
      
      ctest <- cor.test(xVar, yVar, method = tests, alternative = "greater", 
                        conf.level = confidenceInterval)
      
    } else if (hypothesis == "correlatedNegatively") {
      
      ctest <- cor.test(xVar, yVar, method = tests, alternative = "less", 
                        conf.level = confidenceInterval)
    }
    
  }
  
  if (any(tests == "pearson") & length(tests) == 1 && CIPossible) {
    
    CIlow <- formatC(round(ctest$conf.int[1], 1), format = "f", digits = 1)
    CIhigh <- formatC(round(ctest$conf.int[2], 1), format = "f", digits = 1)
    
    text(1, 0.8, labels = paste("ci: [", CIlow, ", ", CIhigh, "]", sep = ""), cex = 2.0)#CI)
    #      text(1, 0.8, labels = paste(100 * confidenceInterval, "% CI: [",
    #         CIlow, ", ", CIhigh, "]", sep = ""), cex = cexCI)
  }
  
}



