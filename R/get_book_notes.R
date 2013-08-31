## get some infomation of a note by note url
.get_book_note0<-function(u,fresh,verbose,...){
  p<-.refreshURL(u,fresh, verbose)
  title<-gsub('[\n ]','',sapply(getNodeSet(p, '//title'), xmlValue))
  n1<-getNodeSet(p, '//div[@class="article"]//div[@class="info"]//a')
  author<-sapply(n1,xmlValue)[1]
  author_uri<-sapply(n1,function(x) xmlGetAttr(x, "href"))[1]
  published<-sapply(getNodeSet(p, '//span[@class="pubtime"]'), xmlValue)
  note<-sapply(getNodeSet(p, '//pre[@id="link-report"]'), xmlValue)
  
  rating<-sapply(getNodeSet(p, '//div[@class="mod profile clearfix"]//span[@class]'),
                 function(x) xmlGetAttr(x, "class"))[2]
  rating<-gsub('[^0-9]','',rating)
  
  v2<-sapply(getNodeSet(p, '//p[@class="pl info"]//span'), xmlValue)
  v2<-gsub("[^0-9]","",v2)
  readers<-v2[1]
  collectors<-v2[2]
  
  out<-c(note_uri=u,title=title,published=published,author=author,
         author_uri=author_uri,note=note,rating=rating,
         readers=readers,collectors=collectors)
  return(out)
}
###############################################################
get_book_notes<-function(bookid,results=100,fresh=10,count=10,verbose=TRUE,...){
  u=paste0('http://book.douban.com/subject/',bookid,'/annotation')
  p<-.refreshURL(u,fresh, verbose)
  total<-sapply(getNodeSet(p, '//title'), xmlValue)
  total<-gsub("^([^)(]*)","",total)
  total<-as.integer(gsub("[^0-9]","",total))
  cat('\n------------There is a total of',total,'notes.------------\n\n')
  pages<-ceiling(min(total,results)/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 9), stringsAsFactors = F)
  colnames(out) <- c("note_uri", "title", "published", "author", "author_uri", "note", 
                     "rating", "readers", "collectors")
  ## output dataFrame nrow index
  k = 1
  for(pg in 1:pages){
    #cat('Getting',(pg-1)*10+1,'--',pg*10,'notes...\n')
    u=paste0('http://book.douban.com/subject/',bookid,
             '/annotation?sort=rank&start=',(pg-1)*count)
    cat("Getting note URLs from page",pg,": ",u,"...\n")
    p<-.refreshURL(u,fresh, verbose)
    href<-sapply(getNodeSet(p, '//div[@class="nlst"]//a[@href]'),
           function(x) xmlGetAttr(x, "href"))
    href<-unique(href[grep("/annotation/",href)])
    href <- href[!href %in% out$note_uri]
    n=length(href)
    if(n>0){
      for(i in 1:n){
        u0<-href[i]
        if(verbose==TRUE){
          cat("   Getting ", k, " book note from URL: ", u0, " ...\n")
        }
        out0<-.get_book_note0(u=u0,fresh,verbose)
        if(length(out0)==9){
          out[k,]<-out0
          k=k+1
        }
        else{
          cat("  !!!! Getting  failed at URL: ", u0, " \n")
        }
      }
    } 
  }
  out <- out[!is.na(out[, 1]), ]
  return(out)
}
