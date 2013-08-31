
.get_book_review0<-function(u,fresh=10,verbose=TRUE,...){
  p<-.refreshURL(u=u,fresh,verbose)
  ## title
  title<-sapply(getNodeSet(p, '//title'), xmlValue)
  title<-gsub("[\n ]","",title)
  ## 作者及其URI
  n1 <- getNodeSet(p, '//div[@class="article"]//div[@class="piir"]//a')[1]
  author<-sapply(n1, xmlValue)
  author_uri<-sapply(n1, function(x) xmlGetAttr(x, "href"))  
  ##评分
  rating<-sapply(getNodeSet(p, '//span[@property="v:rating"]'), xmlValue)
  ##评论发表时间
  published<-sapply(getNodeSet(p, '//span[@property="v:dtreviewed"]'), xmlValue)
  ##评论内容
  review<-sapply(getNodeSet(p, '//span[@property="v:description"]'), xmlValue)
  review<-gsub("\r","",review)
  ## useful and unuseful
  useful<-sapply(getNodeSet(p ,'//span[@class="useful"]//em'), xmlValue)
  unuseful<-sapply(getNodeSet(p ,'//span[@class="unuseful"]//em'), xmlValue)
  out<-c(review_uri=u,title=title,published=published,author=author,
         author_uri=author_uri,review=review, rating=rating,
         useful=useful,unuseful=unuseful) 
  return(out)
}

get_book_reviews<-function(bookid,results=100,fresh=10,count=25,verbose=TRUE,...){
  u=paste0('http://book.douban.com/subject/',bookid,'/reviews')  
  p<-.refreshURL(u,fresh,verbose)
  total<-sapply(getNodeSet(p, '//span[@class="count"]'), xmlValue)
  total<-as.integer(gsub("[^0-9]","",total))
  pages<-ceiling(min(results,total)/count)
  
  cat('\n----------------There is a total of',total,'reviews.----------------\n\n')
  ## 预定义输出dataFrame大小
  out <- data.frame(matrix(nrow = pages * count, ncol = 9), stringsAsFactors = F)
  colnames(out) <- c("review_uri", "title", "published", "author", "author_uri", "review", 
                     "rating", "useful", "unuseful")
  ## out nrow index
  k = 1
  
  for(pg in 1:pages){
    #cat(' Getting',(pg-1)*25+1,'--',pg*25,'comments...\n')
    u=paste0('http://book.douban.com/subject/',bookid,'/reviews?score=&start=',
             (pg-1)*count)
    cat("Getting review URLs from page ", pg, ": ", u, " ...\n")
    p<-.refreshURL(u,fresh,verbose)
    n1<-getNodeSet(p, '//div[@class="ctsh"]//div//a')
    href<-sapply(n1, function(x) xmlGetAttr(x, "href"))
    href<-unique(href[grep("/review/",href)])
    href <- href[!href %in% out$review_uri]
    n<-length(href)
    if(n>0){
      for(i in 1:n){
        u0<-href[i]
        if (verbose == TRUE) {
          cat("   Getting ", k, " movie review from URL: ", u0, " ...\n")
        }
        out0<-.get_book_review0(u=u0,fresh,verbose)
        if(length(out0)==9){
          out[k,]<-out0
          k<-k+1
        } else {
          cat("  !!!! Getting  failed at URL: ", u0, " \n")
        }    
      }
      
    }
  }

  out <- out[!is.na(out[, 1]), ]
  return(out)
}

