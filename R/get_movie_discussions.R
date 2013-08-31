#http://movie.douban.com/subject/5308265/discussion/
##x<-get_movie_discussions(movieid=5308265,results = 100)
.get_movie_discussion0<-function(u,fresh,verbose){
  p<-.refreshURL(u,fresh, verbose)
  title<-gsub('[\n ]','',sapply(getNodeSet(p, '//head//title'),xmlValue))
  published<-sapply(getNodeSet(p, '//div[@class="article"]//span[@class="mn"]'),xmlValue)
  published<-gsub("\n|  ","", published)
  n1<-getNodeSet(p, '//div[@class="article"]//span[@class="pl2"]//a')
  author<-gsub("[\n ]","",sapply(n1,xmlValue)[1])
  author_uri<-sapply(n1,function(x) xmlGetAttr(x, "href"))[1]
  dicussion<-sapply(getNodeSet(p, '//div[@class="article"]//span[@class=""]')[1],xmlValue)
  useful<-sapply(getNodeSet(p, '//span[@class="useful"]//em'),xmlValue)
  unuseful<-sapply(getNodeSet(p, '//span[@class="unuseful"]//em'),xmlValue)
  out<-c(dicussion_uri=u,title=title,published=published,author=author,
         author_uri=author_uri,dicussion=dicussion,useful=useful,unuseful=unuseful)
  return(out)
}
#################################################
get_movie_discussions<-function(movieid,results = 100, fresh = 10,count=20, verbose = TRUE,...){
  u = paste0("http://movie.douban.com/subject/", movieid, "/discussion/")
  p <- .refreshURL(u, fresh, verbose)
  total<-gsub('[^0-9]','',sapply(getNodeSet(p, '//span[@class="count"]'),xmlValue))
  if (length(total)==0)
    stop('There is no discussions about this movie.')
  cat('-----There is a tatal of ',total,' movie discussions.-----\n')
  pages<-ceiling(min(results,as.integer(total))/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 8), stringsAsFactors = F)
  colnames(out) <- c("dicussion_uri", "title", "published", "author", "author_uri", 
                     "dicussion", "useful", "unuseful")
  k=1
  if(pages>0){
    for(pg in 1:pages){
      u=paste0('http://movie.douban.com/subject/',movieid,
                    '/discussion/?start=',(pg-1)*count,'&sort=vote/')
      if(verbose==TRUE) {
        #cat('Getting',(pg-1)*20+1,'--',pg*20,'discussions...\n')
        cat("Getting movie discussion URLS from page=",pg,": ",u,"...\n")
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
          out0<-.get_movie_discussion0(u=u0,fresh,verbose)
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