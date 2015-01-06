
get_book_info<-function(bookid,...){
  u=paste0('http://book.douban.com/subject/',bookid,'/')
  p<-.refreshURL(u)
  ###title
  title<-sapply(getNodeSet(p, '//span[@property="v:itemreviewed"]'), xmlValue)
  ## author ISBN ...
  author<-gsub('[\n ]','',
                  sapply(getNodeSet(p, '//div[@id="info"]//span//a'), xmlValue))
  author<-paste0(author,collapse=' ')
  att1<-sapply(getNodeSet(p, '//div[@id="info"]//span[@class="pl"]'), xmlValue)
  att1<-gsub(' ','',att1)
  att2<-gsub('\n','',sapply(getNodeSet(p, '//div[@id="info"]'), xmlValue))
  attribute<-list()
  ind=which(att1=='\u4f5c\u8005')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
 
  attribute$author<-a1[2]
  ind=which(att1=='\u51fa\u7248\u793e:')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
  attribute$publisher<-a1[2]
  ind=which(att1=='\u51fa\u7248\u5e74:')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
  attribute$pubdate<-a1[2]
  ind=which(att1=='\u9875\u6570:')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
  attribute$pages<-a1[2]
  ind=which(att1=='\u5b9a\u4ef7:')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
  attribute$price<-gsub(' ','',a1[2])
  ind=which(att1=='\u88c5\u5e27:')
  a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
  attribute$binding<-gsub(' ','',a1[2])
  
  ind=which(att1=='\u526f\u6807\u9898:')
  if(length(ind)>0){
    a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
    attribute$subtitle<-gsub(' ','',a1[2])
  }
  ind=which(att1=='\u8bd1\u8005')
  if(length(ind)>0){
    a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
    attribute$translator<-gsub(' |:','',a1[2])
  }
  ind=which(att1=='\u539f\u4f5c\u540d:')
  if(length(ind)>0){
    a1=unlist(strsplit(att2,paste0(att1[ind],'|',att1[ind+1])))
    attribute$origin_title<-gsub(':|  ','',a1[2])
  }
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
  if(length(contentinfo)>2){
    pa<-"" ##...\\(展开全部\\)
    contentinfo<-contentinfo[-grep("...\\(\u5c55\u5f00\u5168\u90e8\\)",contentinfo)]
  }
    
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
  attribute$author_intro<-author_intro
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
