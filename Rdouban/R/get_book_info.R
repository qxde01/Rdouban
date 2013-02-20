
## get douban book infomation
## 

get_book_info<-function(bookid,...){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/')
  pagetree<-getURL(strurl)
  pagetree <- htmlParse(pagetree)
  
  ## author ISBN ...
  booknode <- getNodeSet(pagetree, '//div[@id="info"]')
  bookinfo<-sapply(booknode, xmlValue)
  bookinfo<-gsub('\n|  ','',bookinfo)
  
  ## rating
  votenode <- getNodeSet(pagetree, '//div[@id="interest_sectl"]')
  voteinfo<-sapply(votenode, xmlValue)
  voteinfo<-gsub('\n|','',voteinfo)
  voteinfo<-unlist(strsplit(voteinfo,' '))
  voteinfo<-voteinfo[nchar(voteinfo)>0]
  voteinfo[2]<-gsub('[^0-9]','',voteinfo[2])
  voteinfo<-as.numeric(gsub('%','',voteinfo))
  names(voteinfo)<-c('score','votes','stars5','stars4','stars3','stars2','stars1')
  voteinfo[3:7]<-voteinfo[3:7]/100
  ##content introduction,author introduction
  contentnode <- getNodeSet(pagetree, '//div[@class="intro"]')
  contentinfo<-sapply(contentnode, xmlValue)
  if(length(contentinfo)>2)
    contentinfo<-contentinfo[-grep('...(全部展开)',contentinfo)]
  contentinfo<-gsub('\n| ','',contentinfo)
  clen=length(contentinfo)
  content_intro<-contentinfo[1]
  author_intro<-contentinfo[2]
  
  ##labels
  labelnode <- getNodeSet(pagetree, '//div[@id="db-tags-section"]')
  labelinfo<-sapply(labelnode, xmlValue)
  labelinfo<-gsub(' ','',labelinfo)
  write(labelinfo,'labelinfo.txt')
  labelinfo<-readLines('labelinfo.txt');file.remove('labelinfo.txt')
  labelinfo<-unlist(strsplit(gsub('<[^><]*>|\n','',labelinfo),'\\(|\\)'))
  label_all_number<-as.integer(gsub('[^0-9]','',labelinfo[2]))
  labelinfo<-labelinfo[c(-1,-2,-3)]
  
  label_name<-labelinfo[-grep('[0-9]',labelinfo)]
  label_freq<-as.integer(labelinfo[grep('[0-9]',labelinfo)])
  ## reader info
  readnode <- getNodeSet(pagetree, '//p[@class="pl"]//a[@href]')
  readinfo<-sapply(readnode, xmlValue)
  readinfo<-readinfo[3:5]
  readerinfo<-as.integer(gsub('[^0-9]','',readinfo))
  names(readerinfo)<-c('doings','collections','wishes')
  
  list(book_info=bookinfo,
       votes_info=voteinfo,
       content_intro=content_intro,
       author_intro=author_intro,
       label_all_number=label_all_number,
       labels=data.frame(label_name,label_freq),
       reader_info=readerinfo)
}
