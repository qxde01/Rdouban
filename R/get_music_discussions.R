##http://music.douban.com/subject/3843530/discussion/
##musicid=3843530
.get_music_discussion0<-function(u,fresh,verbose){
  .get_movie_discussion0(u,fresh,verbose)
}
#################################################################
get_music_discussions<-function(musicid,results = 100, fresh = 10,count=20, verbose = TRUE,...){
  u = paste0("http://music.douban.com/subject/", musicid, "/discussion/")
  p <- .refreshURL(u, fresh, verbose)
  total<-gsub('[^0-9]','',sapply(getNodeSet(p, '//span[@class="count"]'),xmlValue))
  if (length(total)==0)
    stop('There is no discussions about this music.')
  cat('-----There is a tatal of ',total,' music discussions.-----\n')
  pages<-ceiling(min(results,as.integer(total))/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 8), stringsAsFactors = F)
  colnames(out) <- c("dicussion_uri", "title", "published", "author", "author_uri", 
                     "dicussion", "useful", "unuseful")
  k=1
  if(pages>0){
    for(pg in 1:pages){
      u=paste0('http://music.douban.com/subject/',musicid,
               '/discussion/?start=',(pg-1)*count,'&sort=vote/')
      if(verbose==TRUE) {
        #cat('Getting',(pg-1)*20+1,'--',pg*20,'discussions...\n')
        cat("Getting music discussion URLS from page=",pg,": ",u,"...\n")
      }
      p <- .refreshURL(u, fresh, verbose)
      n1<-getNodeSet(p ,'//table[@class="olt"]//td/a')
      href<-unique(sapply(n1,function(x) xmlGetAttr(x, "href")))
      href<-href[grep('/discussion/',href)]
      href <- href[!href %in% out$dicussion_uri]
      n=length(href)
      if(n>0){
        for(i in 1:n){
          u0<-href[i]
          if(verbose==TRUE){
            cat("   Getting ", k, " movie discussion from URL: ", u0, " ...\n")
          }
          out0<-.get_music_discussion0(u=u0,fresh,verbose)
          if(length(out0)==8){
            out[k,]<-out0
            k=k+1
          }
          else{
            cat("  !!!! Getting  failed at URL: ", u0, " \n")
          }       
        }
      }
    }
  }
  out <- out[!is.na(out[, 1]), ]
  return(out)
}

#a<-get_music_discussions(musicid=3843530,results = 100)