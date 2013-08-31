get_movie_info<-function(movieid,...){
  u=paste0('http://movie.douban.com/subject/',movieid,'/')
  p<-.refreshURL(u)
  #pagetree<-htmlParse(getURL(strurl))
  ##base infomation
  author <-sapply(getNodeSet(p,'//div[@id="info"]//a[@rel="v:directedBy"]'),xmlValue)
  #stars <-sapply(getNodeSet(p,'//div[@id="info"]//a[@rel="v:starring"]'),xmlValue)
  #genre <-sapply(getNodeSet(p,'//div[@id="info"]//span[@property="v:genre"]'),xmlValue)
  #runtime<-sapply(getNodeSet(p,'//div[@id="info"]//span[@property="v:runtime"]'),xmlValue)
  title<-sapply(getNodeSet(p, '//span[@property="v:itemreviewed"]'), xmlValue)
  attribute<-gsub('[\n ]','',sapply(getNodeSet(p,'//div[@id="info"]'), xmlValue))
  ##rating
  rating<-sapply(getNodeSet(p, '//div[@id="interest_sectl"]'), xmlValue)
  rating<-unlist(strsplit(gsub('\n','',rating),' '))
  rating<-rating[nchar(rating)>0]
  rating[2]<-gsub('[^0-9]','',rating[2])
  rating<-as.numeric(gsub('%','',rating))
  if(length(rating)<7){
    rating<-c(rating,rep(0,7-length(rating)))
  }
  rating[3:7]<-rating[3:7]/100
  names(rating)<-c('average','votes','stars5','stars4','stars3','stars2','stars1')
  ##the introduction of movie
  intronode <- getNodeSet(p, '//span[@class="all hidden"]')
  if(length(intronode)==0)
    intronode <- getNodeSet(p, '//span[@property="v:summary"]')
  summary<-gsub("[\n ]","",sapply(intronode, xmlValue))
  
  ## tags
  labelinfo<-sapply(getNodeSet(p, '//div[@id="db-tags-section"]//div'), xmlValue)
  if(length(labelinfo)==0)
    labelinfo<-sapply(getNodeSet(p, '//div[@class="tags-body"]//a'), xmlValue)
  labelinfo<-unlist(strsplit(labelinfo,' |\\(|\\)'))
  tags<-data.frame(tag_label=labelinfo[seq(1,length(labelinfo),2)],
                   tag_freq=labelinfo[seq(2,length(labelinfo),2)],
                   stringsAsFactors=F)
  ## total of reviews
  reviews_total<-gsub("[^0-9]","",sapply(getNodeSet(p, '//div[@class="review-more"]//a'), xmlValue))
  #total of comments
  comments_total<-gsub('[^0-9]','',
                      sapply(getNodeSet(p, '//div[@id="comments-section"]//span//a')[1], xmlValue))
  #if(length(short_comments)==0)short_comments<-0
  #if(length(long_comments)==0)long_comments<-0
  ##audience
  audience<-sapply(getNodeSet(p, '//div[@class="subject-others-interests-ft"]//a'), xmlValue)
  audience<-as.integer(gsub('[^0-9]','',audience))
  if(length(audience)==3)
    names(audience)<-c('doings','collections','wishes')

  image<-sapply(getNodeSet(p, '//div[@class="indent"]//div[@id="mainpic"]//img'),
                function(x) xmlGetAttr(x, "src"))

  list(title=title,author=author,
       rating=rating,
       summary=summary,
       tags=tags,
       href=u,image=image,
       reviews_total=reviews_total,
       comments_total=comments_total,
       audience=audience,attribute=attribute)
}
## get_movie_info(movieid=10527209)
