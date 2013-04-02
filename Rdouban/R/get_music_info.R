##http://music.douban.com/subject/3843530/
##musicid=3843530
get_music_info<-function(musicid,...){
  strurl=paste0('http://music.douban.com/subject/',musicid,'/')
  pagetree <- htmlParse(getURL(strurl))
  title<-gsub('[\n ]|\\(豆瓣|\\)','',sapply(getNodeSet(pagetree, '//head//title'),xmlValue))
  base_info<-gsub('[\n]','',sapply(getNodeSet(pagetree, '//div[@id="info"]'),xmlValue))
  base_info<-gsub('\x810\x842|   ',' ',iconv(base_info,from='UTF-8',to='GB18030'))
  
  track<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//table[@class="olts"]//tr//td'),xmlValue))
  track<-track[nchar(track)>0][c(-1,-2)]
  
  music_intro<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//span[@class="all hidden"]'),xmlValue))
  if(length(music_intro)==0)
    music_intro<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//span[@property="v:summary"]'),xmlValue))
  
  
  labels<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//div[@id="db-tags-section"]'),xmlValue))
  labels<-iconv(labels,from='UTF-8',to='GB18030')
  labels<-gsub('\x810\x842','',labels)
  labels<-gsub('··','',unlist(strsplit(labels,'\\(|\\)'))[-1])
  labels_amount<-as.integer(gsub('[^0-9]','',labels[1]))
  labels_name<-labels[-1][seq(1,length(labels[-1]),2)]
  labels_freq<-as.integer(labels[-1][seq(2,length(labels[-1]),2)])
  
  rating<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//div[@id="interest_sectl"]'),xmlValue))
  
  rating<-gsub('\\([^\\(\\)]*\\)|%',' ',rating)
  rating<-unlist(strsplit(rating,' '))
  rating<-as.numeric(rating)
  rating[3:7]<-rating[3:7]/100
  names(rating)<-c('score','votes_amount','stars5','stars4','stars3','stars2','stars1')
  
  comments<-gsub('[\n ]','',sapply(getNodeSet(pagetree, '//p[@class="pl"]//a'),xmlValue))
  comments<-gsub('[^0-9]|feed:rss2.0','',comments)
  comments<-comments[nchar(comments)>0]
  audience<-as.integer(comments[2:5])
  names(audience)<-c('comments_amount','doings','collections','wishes')
  
  list(title=title,
       base_info=base_info,
       track=track,
       music_intro=music_intro,
       labels_amount=labels_amount,
       labels=data.frame(labels_name=labels_name,
                         labels_freq=labels_freq,
                         stringsAsFactors=F),
       rating=rating,
       audience=audience)
}

##x<-get_music_info(musicid=1404439)