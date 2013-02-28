##movieid=5308265
## x=get_movie_reviews(movieid=5308265,n=50)
get_movie_reviews<-function(movieid,n=100,...){
  strurl=paste0('http://movie.douban.com/subject/',movieid,'/reviews')
  pagetree <- htmlParse(getURL(strurl))
  
  title0<- sapply(getNodeSet(pagetree, '//title'),xmlValue)
  title<-gsub('[0-9 \n\\(\\)]|µÄÓ°ÆÀ','',title0)
  reviews_amount<-as.integer(gsub('[^0-9]','',title0))
  
  rating<-sapply(getNodeSet(pagetree, '//div[@class="rating_list clearfix"]//span'),xmlValue)[-1]
  rating<-as.integer(gsub('[0-5]ÐÇ|[ -]','',rating))
  names(rating)<-c('stars5','stars4','stars3','stars2','stars1')
    
  cat('There is a total of',reviews_amount,'reviews...\n')
  .get_review<-function(pagetree){
    urlsnode<-getNodeSet(pagetree, '//div[@class="ctsh"]//a')
    urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
    review_url<-urls[grep('/review/',urls)]
    author_url<-urls[grep('/people/',urls)]
    #urlsvalue<-gsub('[\n ]','',sapply(urlsnode,xmlValue))
    #urlsvalue<-urlsvalue[nchar(urlsvalue)>0]
    
    m=length(review_url)
    rev<-c()
    for(i in 1:m){
      cat('Getting',review_url[i],'reviews infomation...\n')
      reviewtree <- htmlParse(getURL(review_url[i]))
      title <- sapply(getNodeSet(reviewtree, '//span[@property="v:summary"]'),xmlValue)
      time<-sapply(getNodeSet(reviewtree, '//span[@property="v:dtreviewed"]'),xmlValue)
      nickname<-sapply(getNodeSet(reviewtree, '//span[@property="v:reviewer"]'),xmlValue)
      rating<-sapply(getNodeSet(reviewtree, '//span[@property="v:rating"]'),xmlValue)
      review<-sapply(getNodeSet(reviewtree, '//span[@property="v:description"]'),xmlValue)
      useful<-sapply(getNodeSet(reviewtree, '//span[@class="useful"]//em'),xmlValue)
      unuseful<-sapply(getNodeSet(reviewtree, '//span[@class="unuseful"]//em'),xmlValue)
      rev0<-c(title=title,review=review,
        time=time,nickname=nickname,
        rating=as.integer(rating),
        useful=as.integer(useful),unuseful=as.integer(unuseful),
        reviews_url=review_url[i],authors_url=author_url[i])
      rev<-rbind(rev,rev0)
    }  
    row.names(rev)<-NULL
    rev
  }
  pages<-ceiling(min(n,reviews_amount)/20)
  reviews_info<-.get_review(pagetree)
  if(pages>1){
    for(pg in 2:pages){
      cat(' Getting',(pg-1)*20+1,'--',pg*20,'reviews...\n')
      strurl=paste0('http://movie.douban.com/subject/',movieid,
                    '/reviews?start=',(pg-1)*20,'&filter=&limit=20')
      pagetree <- htmlParse(getURL(strurl))
      reviews_info0<-.get_review(pagetree)
      reviews_info<-rbind(reviews_info,reviews_info0)
    }
  }
  row.names(reviews_info)<-NULL
  list(movie_title=title,
       reviews_amount=reviews_amount,
       rating=rating,
       reviews_info=reviews_info)
}