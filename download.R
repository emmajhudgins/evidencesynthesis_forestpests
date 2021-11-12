# Code adapted from Dat Nguyen's funcs.R
#function that actually attempts the scrape and returns 1 or 0 depending on success
review_scopus_urls = read.csv(file = './review_scopus_urls.csv')
evidence_wos_urls = read.csv(file = './evidence_wos_urls.csv')

isolate<-function(x,b,e) #using strsplit to isolate string
{
  
  tmp<-strsplit(x,b)
  tmp2<-strsplit(tmp[[1]][2],e) #take text after b
  tmp3<-tmp2[[1]][1] #take text before e
  return(tmp3)
}

attemptPDF <- function(url,row,type){
  print(paste("     ATTEMPTING:",type))
  
  require(RCurl)
  require(downloader)
  
  skipIsolate <- 0
  ### SPECIFIC CASE HANDLING ###
  if(grepl("doi/abs/",url)){
    p <- url
    p <- gsub("abs", "pdf", p)
    p <- paste(p, "?needAccess=true", sep = "")
    skipIsolate <- 1
  }
  if(grepl("tandfonline", url)){
    p <- url
    p <- gsub("abs", "pdf", p)
    p <- gsub("full", "pdf", p)
    p <- paste(p, "?needAccess=true", sep = "")
    skipIsolate <- 1
  }
  if(grepl("emeraldinsight.com/doi/",url)){
    p <- url
    p <- gsub("abs", "pdfplus", p)
    p <- gsub("full", "pdfplus", p)
    p <- gsub("/doi/10","/doi/pdfplus/10",p)
    skipIsolate <- 1
  }
  if(grepl("tandfprod", url)){
    p <- "http://www.tandfonline.com/doi/pdf/"
    p <- paste(p,  gsub(".*abs/","",url), "?needAccess=true", sep = "")
    skipIsolate <- 1
  }
  if(grepl("omicsonline",url)){
    p <- url
    p <- gsub("html", "pdf", p)
    p <- gsub(".digital/", ".digital/fscommand/", p)
    skipIsolate <- 1
  }
  if(grepl("bioone.org/doi",url)){
    p <- url
    p <- gsub("abs","pdf",p)
    p <- gsub("/doi/10","/doi/pdf/10",p)
    skipIsolate <- 1
  }
  #specific case handling for perlserv (2 links in particular)
  if(grepl("bioone.org/perlserv",url)){
    p <- gsub(".*doi=","",url)
    p <- paste("http://www.bioone.org/doi/pdf/",p,sep="")
    skipIsolate <- 1
  }
  
  if(grepl("\\.pdf$",url)){
    p <- url
    print(paste("        ",p))
    r <- tryCatch(download(p,paste(row,".pdf",sep=""),quiet=T),warning = function(w) w[[1]], error = function(e) "**ERROR:getting file from pdf")
    if(r != 0 & skipIsolate==0){
      return(paste(0,r,sep=""))
    } else {
      return(1)
    }
  }
  
  if(grepl("degruyter.com/view/j",url)){
    p <- url
    p <- gsub(".com/view/j",".com/downloadpdf/j",p)
    skipIsolate <- 1
  }
  
  if(grepl("annualreviews",url)){
    p <- url
    p <- gsub("/doi/","/doi/pdf/",p)
    skipIsolate <- 1
  }
  
  if(grepl("jstor",url)){
    p <- url
    p <- gsub("\\?origin=crossref","",p)
    p <- gsub("/stable/","/stable/pdf/",p)
    p <- paste(p,".pdf",sep="")
    skipIsolate <- 1
  }
  
  if(grepl("springer",url)){
    p <- url
    p <- gsub('/content/', '/content/pdf', p)
    p <- paste(p,".pdf",sep="")
    skipIsolate <- 1
  }
  
  if(grepl("pubs.cif-ifc",url)){
    p <- url
    p <- gsub('/doi/', '/doi/pdf/', p)
    tryCatch(download.file(p,paste0(row,".pdf")))
    return(1)
  }
  
  #super ultra special case wiley: looking at this code may induce headaches
  if(grepl("doi.wiley",url)){
    #reformat url to the proper url
    psub <- gsub(".*wiley.com/","",url)
    p <- paste("http://onlinelibrary.wiley.com/doi/",psub,"/pdf",sep="")
    r.tmp <- tryCatch(download.file(p,"tmp_file",quiet=T),
                      warning = function(w) w[[1]], 
                      error = function(e) "**ERROR:getting tmp from wiley")
    if(r.tmp != 0){
      return(paste(0,r.tmp,sep=""))
    }
    wholefile <- tryCatch(readChar("tmp_file",file.info("tmp_file")$size), 
                          error = function(e) "Reading tmp_file error")
    if(wholefile == "Reading tmp_file error"){
      return(paste(0,wholefile,sep=''))
    }
    vector.lines <- strsplit(wholefile,"\n")[[1]]
    #the actual pdf frame url has the word "store" in it, so we grep for that
    p.ind <- grep("store",vector.lines)
    if(length(p.ind)==0){
      return("0ERROR: searching Wiley for pdf link")
    }
    p <- vector.lines[p.ind]
    b <- ".*\"pdfDocument\" src=\""
    e <- "\""
    url2 <- isolate(p,b,e)
    print(paste("        ",url2))
    r <- tryCatch(download.file(url2,paste(row,".pdf",sep=""),quiet=T),
                  warning = function(w) w[[1]], error = function(e) 
                    "**ERROR:invalid url (pdf link not found)")
    if(r != 0 & skipIsolate==0){
      return(paste(0,r,sep=""))
    } else {
      return(1)
    }
  }
  
  #try to get tmp_file of url
  r <- tryCatch(download(p,paste0(row,".pdf"),quiet=T), warning = function(w) w[[1]], error = function(e) "**ERROR: downloading tmp_file")
  if(r != 0 & skipIsolate==0){
    return(paste(0,r,sep=""))
  }
  
  return(1)
}



row_index = 0
for(url in evidence_wos_urls[,2]){
  attemptPDF(url,row_index,'url')
  row_index = row_index + 1
}



