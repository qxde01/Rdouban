
get_book_info<-function(bookid,...){
  u=paste0('http://book.douban.com/subject/',bookid,'/')
  p<-.refreshURL(u)
  ###title
  title<-sapply(getNodeSet(p, '//span[@property="v:itemreviewed"]'), xmlValue)
  ## author ISBN ...
  author<-gsub('[\n ]','',
                  sapply(getNodeSet(p, '//div[@id="info"]//a'), xmlValue))[1]
  attribute<-gsub('[\n ]','',
                  sapply(getNodeSet(p, '//div[@id="info"]'), xmlValue))
  ## rating
  score<-as.numeric(gsub('[\n ]','',
                         sapply(getNodeSet(p, '//strong[@property="v:average"]'), xmlValue)))
  votes<-as.integer(sapply(getNodeSet(p, '//span[@property="v:votes"]'), xmlValue))
  if(length(votes)==0)votes<-NA
  rating<-sapply(getNodeSet(p, '//div[@id="interest_sectl"]//div'), xmlValue)
  rating<-gsub('[\n%]|\\([^\\(\\)]*\\)','',rating)
  rating<-unlist(strsplit(rating,' '))
  rating<-rating[nchar(rating)>0]
  rating<-as.numeric(rating[nchar(rating)>0])[-1]/100
  if(length(rating)<5)rating<-c(rating,rep(NA,5-length(rating)))
  rating<-c(score,votes,rating)
  names(rating)<-c('average','votes','stars5','stars4','stars3','stars2','stars1')
  
  medium<-sapply(getNodeSet(p, '//div[@class="indent"]//div[@id="mainpic"]//a'),
                 function(x) xmlGetAttr(x, "href"))
  large<-sapply(getNodeSet(p, '//div[@class="indent"]//div[@id="mainpic"]//img'),
                function(x) xmlGetAttr(x, "src"))
  image<-c(medium =medium ,large=large)
  ##content introduction,author introduction
  contentinfo<-gsub('[\n ]','',sapply(getNodeSet(p, '//div[@class="intro"]'),
                                     xmlValue))
  if(length(contentinfo)>2)
    contentinfo<-contentinfo[-grep("...\\(展开全部\\)",contentinfo)]
  #clen=length(contentinfo)
  summary<-contentinfo[1]
  author_intro<-contentinfo[2]
  
  ##labels
  #labels_amount <- gsub('[^0-9]','',sapply(
  #  getNodeSet(p, '//div[@id="db-tags-section"]//h2'),xmlValue))
  labelinfo<-gsub('\n','',
                  sapply(getNodeSet(p, '//div[@id="db-tags-section"]//div'), xmlValue))
  labelinfo<-unlist(strsplit(labelinfo,' '))
  #labelinfo<-gsub('<[^<>]*>|\n','',labelinfo)
  labelinfo<-labelinfo[nchar(labelinfo)>1]
 
  .label_tran<-function(x){
    x=strsplit(x,'[\\(\\)]')
    n=length(x)
    tag_label<-tag_freq<-c()
    for(i in 1:n){
      temp<-x[[i]]
      temp<-temp[nchar(temp)>0]
      tag_label[i]<-temp[length(temp)-1]
      tag_freq[i]<-temp[length(temp)]
    }
   data.frame(tag_label,tag_freq,stringsAsFactors=F) 
  }
  tags<-.label_tran(labelinfo)

  ## reader info
  readnode <- getNodeSet(p, '//div[@class="indent"]//p[@class="pl"]//a')
  readers<-as.integer(gsub('[^0-9]','',sapply(readnode, xmlValue)))
  if(length(readers)<3)readers<-c(readers,rep(NA,3-length(readers)))
  names(readers)<-c('doings','collections','wishes')
  ##
  comments_total<-gsub('[^0-9]','',
                       sapply(getNodeSet(p, '//div[@id="reviews"]//h2'), xmlValue))
  notes_total<-gsub('[^0-9]','',sapply(getNodeSet(p, '//div[@class="hd"]'), xmlValue))
  
  list(title=title,
       author=author,
       rating=rating,
       href=u,
       summary=summary,
       author_intro=author_intro,
       image=image,
       tags=tags,
       comments_total=comments_total,
       notes_total=notes_total,
       readers=readers,
       attribute=attribute)
}
