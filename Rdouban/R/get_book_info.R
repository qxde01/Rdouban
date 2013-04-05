
get_book_info<-function(bookid,...){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/')
  pagetree <- htmlParse(getURL(strurl))
  ###title
  book_title<-sapply(getNodeSet(pagetree, '//head//title'), xmlValue)
  book_title<-gsub('\\(豆瓣\\)','',book_title)
  ## author ISBN ...
  booknode <- getNodeSet(pagetree, '//div[@id="info"]')
  base_info<-gsub('\n|    ','',sapply(booknode, xmlValue))
  
  ## rating
  score <- sapply(getNodeSet(pagetree, '//strong[@property="v:average"]'), xmlValue)
  score<-as.numeric(gsub('[\n ]','',score))
  votes<-as.integer(sapply(getNodeSet(pagetree, '//span[@property="v:votes"]'), xmlValue))
  if(length(votes)==0)votes<-NA
  rating<-sapply(getNodeSet(pagetree, '//div[@id="interest_sectl"]//div'), xmlValue)
  rating<-gsub('[\n%]|\\([^\\(\\)]*\\)','',rating)
  rating<-unlist(strsplit(rating,' '))
  rating<-rating[nchar(rating)>0]
  rating<-as.numeric(rating[nchar(rating)>0])[-1]/100
  if(length(rating)<5)rating<-c(rating,rep(NA,5-length(rating)))
  rating<-c(score,votes,rating)
  names(rating)<-c('score','votes','stars5','stars4','stars3','stars2','stars1')
  
  ##content introduction,author introduction
  contentnode <- getNodeSet(pagetree, '//div[@class="intro"]')
  contentinfo<-sapply(contentnode, xmlValue)
  if(length(contentinfo)>2)
    contentinfo<-contentinfo[-grep('...\\(展开全部\\)',contentinfo)]
  contentinfo<-gsub('\n| ','',contentinfo)
  clen=length(contentinfo)
  content_intro<-contentinfo[1]
  author_intro<-contentinfo[2]
  
  ##labels
  labels_amount <- gsub('[^0-9]','',sapply(
    getNodeSet(pagetree, '//div[@id="db-tags-section"]//h2'),xmlValue))
  labelinfo<-sapply(getNodeSet(pagetree, '//div[@id="db-tags-section"]//div'), xmlValue)
  
  #labelinfo<-iconv(labelinfo,from='UTF-8',to='')
  #labelinfo<-gsub("\x810\x842| ",'',labelinfo)
  #labelinfo<-iconv(labelinfo,from='GB18030',to='UTF-8')
  labelinfo<-unlist(strsplit(labelinfo,' '))
  #labelinfo<-unlist(strsplit(labelinfo,'[ \\(\\)]'))
  labelinfo<-gsub('<[^<>]*>','',labelinfo)
  labelinfo<-labelinfo[nchar(labelinfo)>0]
  labelinfo<-labelinfo[grep('[0-9]',labelinfo)]
  label_tran<-function(x){
    x=strsplit(x,'[\\(\\)]')
    n=length(x)
    labels_name<-labels_freq<-c()
    for(i in 1:n){
      temp<-x[[i]]
      temp<-temp[nchar(temp)>0]
      labels_name[i]<-temp[length(temp)-1]
      labels_freq[i]<-temp[length(temp)]
    }
   cbind(labels_name,labels_freq) 
  }
  labelinfo<-label_tran(labelinfo)
  #
  #gsub('\\([\\(\\)]*\\)','',labelinfo)
  #labelinfo<-unlist(strsplit(labelinfo,'[\\(\\)]'))
  #labelinfo<-labelinfo[nchar(labelinfo)>0]
  
  labels_name<-labelinfo[,1]
  labels_freq<-labelinfo[,2]
  ## reader info
  readnode <- getNodeSet(pagetree, '//div[@class="indent"]//p[@class="pl"]//a')
  readerinfo<-as.integer(gsub('[^0-9]','',sapply(readnode, xmlValue)))
  if(length(readerinfo)==1)readerinfo<-c(readerinfo,NA,NA)
  if(length(readerinfo)==2)readerinfo<-c(readerinfo,NA)
  names(readerinfo)<-c('doings','collections','wishes')
  ##
  comments_amount<-sapply(getNodeSet(pagetree, '//div[@id="reviews"]//h2'), xmlValue)
  comments_amount<-gsub('[^0-9]','',comments_amount)
  notes_amount<-sapply(getNodeSet(pagetree, '//div[@class="hd"]'), xmlValue)
  notes_amount<-gsub('[^0-9]','',notes_amount)
  if(length(comments_amount)==0)comments_amount<-NA
  if(length(notes_amount)==0)notes_amount<-NA
  comments_notes_amount<-as.integer(c(comments_amount,notes_amount))
  names(comments_notes_amount)<-c('comments_amount','notes_amount')
  
  list(book_title=book_title,
       base_info=base_info,
       raing=rating,
       content_intro=content_intro,
       author_intro=author_intro,
       labels_amount=as.integer(labels_amount),
       labels=data.frame(labels_name,labels_freq=as.integer(labels_freq),stringsAsFactors=F),
       comments_notes_amount=comments_notes_amount,
       reader_info=readerinfo)
}
