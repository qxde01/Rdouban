##http://music.douban.com/subject/3843530/
##musicid=3843530
get_music_info<-function(musicid,...){
  u=paste0('http://music.douban.com/subject/',musicid,'/')
  p<-.refreshURL(u)
  pa<-'[\n ]|\\(\u8c46\u74e3|\\)' ##[\n ]|\\(豆瓣|\\)
  Encoding(pa)<-"UTF-8"
  title<-gsub(pa,'',sapply(getNodeSet(p, '//head//title'),xmlValue))
  attribute<-gsub('[\n]|   ','',sapply(getNodeSet(p, '//div[@id="info"]'),xmlValue))
  author<-sapply(getNodeSet(p, '//div[@id="info"]//span[@class="pl"]//a'),xmlValue)[1]
  ##
  song<-sapply(getNodeSet(p, '//ul[@class="song-items"]//div[@data-title]'),xmlValue)
  track<-sapply(getNodeSet(p, '//ul[@class="song-items"]//span[@class="n_doulists unfoldable"]'),
                     xmlValue)
  songs<-data.frame(title=song,track=track,stringsAsFactors=F)
  ##
  summary <-gsub('[\n ]','',sapply(getNodeSet(p, '//span[@class="all hidden"]'),xmlValue))
  if(length(summary )==0)
    summary <-gsub('[\n ]','',sapply(getNodeSet(p, '//span[@property="v:summary"]'),xmlValue))
  ##
  
  labels<-gsub('[\n ]','',
               sapply(getNodeSet(p, '//div[@id="db-tags-section"]//div'),xmlValue))
  labels<-unlist(strsplit(labels,'\\(|\\)'))
  tags<-data.frame( tag_label=labels[seq(1,length(labels)-1,2)],
                    tag_freq=as.integer(labels[seq(2,length(labels),2)]),
                    stringsAsFactors=F)
 # tags$tag_label<-gsub("<U+00A0>","",tags$tag_label)

  rating<-gsub('[\n ]','',sapply(getNodeSet(p, '//div[@id="interest_sectl"]'),xmlValue))
  rating<-gsub("\u4eba\u8bc4\u4ef7",'',rating) #\u4eba\u8bc4\u4ef7 人评价
  rating<-as.numeric(unlist(strsplit(rating,'\\(|\\)|%')))
  rating[-c(1,2)]<-rating[-c(1,2)]/100
  if(length(rating)==7)
    names(rating)<-c('average','votes','stars5','stars4','stars3','stars2','stars1')
  ##
  comments_total<-gsub('[^0-9]','',sapply(getNodeSet(p, '//div//p[@class="pl"]//a'),xmlValue))[1]
  listeners<-sapply(getNodeSet(p, '//div[@id="collector"]//p//a'),xmlValue)
  listeners<-gsub("[^0-9]","",listeners)
  names(listeners)<-c('doings','collections','wishes')

  large <- sapply(getNodeSet(p, '//div[@id="mainpic"]//a'), 
                   function(x) xmlGetAttr(x, "href"))
  medium <- sapply(getNodeSet(p, '//div[@id="mainpic"]//img'), 
                  function(x) xmlGetAttr(x, "src"))
  image <- c(medium = medium, large = large)
  list(title=title,
       author=author,rating=rating,
       summary=summary,
       tags=tags, songs=songs,
       href=u,image=image,
       comments_total=comments_total,
       listeners=listeners,attribute=attribute)
}

##x<-get_music_info(musicid=1404439)