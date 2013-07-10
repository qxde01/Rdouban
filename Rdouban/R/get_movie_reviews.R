##movieid=5308265
## x=get_movie_reviews(movieid=5308265,n=20)
get_movie_reviews<-function(movieid,n=100,verbose=TRUE,...){
  strurl=paste0('http://movie.douban.com/subject/',movieid,'/reviews')
  pagetree <- htmlParse(getURL(strurl))
  
  title0<- sapply(getNodeSet(pagetree, '//head//title'),xmlValue)
  title<-gsub('[0-9 \n\\(\\)]|的影评|的评论','',title0)
  reviews_amount<-as.integer(gsub('[^0-9]','',title0))
  
  rating<-sapply(getNodeSet(pagetree, '//div[@class="rating_list clearfix"]//span'),xmlValue)[-1]
  rating<-as.integer(gsub('[0-5]星|[ -]','',rating))
  names(rating)<-c('stars5','stars4','stars3','stars2','stars1')
  
  cat('There is a total of',reviews_amount,'reviews...\n')
  
  .get_review<-function(pagetree,verbose=TRUE,...){
    urlsnode<-getNodeSet(pagetree, '//div[@class="review"]//a')
    urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
    review_url<-urls[grep('/review/',urls)]
    author_url<-urls[grep('/people/',urls)]
    #urlsvalue<-gsub('[\n ]','',sapply(urlsnode,xmlValue))
    #urlsvalue<-urlsvalue[nchar(urlsvalue)>0]
    
    m=length(review_url)
    rev<-c()
    for(i in 1:m){
      if(verbose==TRUE)
        cat('  Getting long comments from ',review_url[i],'...\n')
      
      reviewtree <- htmlParse(getURL(review_url[i]))
      title <- sapply(getNodeSet(reviewtree, '//span[@property="v:summary"]'),xmlValue)
      time<-sapply(getNodeSet(reviewtree, '//span[@property="v:dtreviewed"]'),xmlValue)
      nickname<-sapply(getNodeSet(reviewtree, '//span[@property="v:reviewer"]'),xmlValue)
      rating<-sapply(getNodeSet(reviewtree, '//span[@property="v:rating"]'),xmlValue)
      review<-sapply(getNodeSet(reviewtree, '//span[@property="v:description"]'),xmlValue)
      if(length(review)==0)
        review<-sapply(getNodeSet(reviewtree, '//div[@property="v:description"]'),xmlValue)
      useful<-sapply(getNodeSet(reviewtree, '//span[@class="useful"]//em'),xmlValue)
      unuseful<-sapply(getNodeSet(reviewtree, '//span[@class="unuseful"]//em'),xmlValue)
      if(length(useful)==0|length(unuseful)==0){
        x0<-sapply(getNodeSet(reviewtree, '//div[@class="main-panel-useful"]//em'),xmlValue)
        useful=x0[1]
        unuseful=x0[2]
      }
      
      rev0<-c(title,review,time,nickname,rating,
              useful,unuseful,review_url[i],author_url[i])
      rev<-rbind(rev,rev0)
    }  
    row.names(rev)<-NULL
    rev
  }
  pages<-ceiling(min(n,reviews_amount)/20)
  reviews_info<-.get_review(pagetree,verbose=verbose)
  if(pages>1){
    for(pg in 2:pages){
      cat('Getting',(pg-1)*20+1,'--',pg*20,'reviews...\n')
      strurl=paste0('http://movie.douban.com/subject/',movieid,
                    '/reviews?start=',(pg-1)*20,'&filter=&limit=20')
      pagetree <- htmlParse(getURL(strurl))
      reviews_info0<-.get_review(pagetree,verbose=verbose)
      reviews_info<-rbind(reviews_info,reviews_info0)
    }
  }
  row.names(reviews_info)<-NULL
  reviews_info<-data.frame(title=reviews_info[,1],
                           review=reviews_info[,2],
                           time=reviews_info[,3],
                           nickname=reviews_info[,4],
                           rating=as.integer(reviews_info[,5]),
                           useful=as.integer(reviews_info[,6]),
                           unuseful=as.integer(reviews_info[,7]),
                           review_url=reviews_info[,8],
                           author_url=reviews_info[,9],
                           stringsAsFactors=F)
  
  list(movie_title=title,
       reviews_amount=reviews_amount,
       rating=rating,
       reviews_info=reviews_info)
}