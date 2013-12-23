##' source('user_book_viz.R',encoding='UTF-8')
##' 阅读综合统计
##' @x:collect_df
##' @YEAR:统计年份
##' @NR_REVIEW:书评篇数
##' @NR_NOTE:笔记篇数
##' statSummary(x=collect_df2)
statSummary<-function(x,YEAR=NULL,NR_REVIEW=0,NR_NOTE=0){
  #cat(" #### 阅读综合统计......\n")
  cat(" #### \u9605\u8bfb\u7efc\u5408\u7edf\u8ba1......\n")
  n=nrow(x) #number
  star5num<-nrow(x[x$user_rating=="5",]) # star5 number
  amount<-table(x$month)
  maxMonth<-names(amount[amount==max(amount)])
  maxAmount<-max(amount)
  averageAmount<-(length(amount)*30.42)/sum(amount)
  priceTotal<-sum(as.double(x$price),na.rm = T)
  pageAmount<-tapply(as.integer(x$pages),x$month,sum,na.rm = T)
  maxMonthPage<-names(pageAmount[pageAmount==max(pageAmount)])[1]
  maxAmountPage<-max(pageAmount)[1]
  averageAmountPage<-sum(pageAmount)/(length(pageAmount)*30.42)
  ###############################################
  ##' 共阅读过,\u5171\u9605\u8bfb\u8fc7
  ##' 本书,总价值 \u672c\u4e66,\u603b\u4ef7\u503c
  ##'str1<-paste(YEAR,"共阅读过",n,"本书,总价值",priceTotal,"￥")
  ##' 五星好书 \u4e94\u661f\u597d\u4e66,本 \u672c
  ##' str2<-paste("五星好书",star5num,"本")
  ##' 月最勤快读了,\u6708\u6700\u52e4\u5feb\u8bfb\u4e86
  ##' str3<-paste(maxMonth,"月最勤快读了",maxAmount,"本")
  ##' str4<-paste("平均",round(averageAmount,digits=2),"天读一本书")
  ##' str5<-paste(maxMonthPage,"月最多读了",maxAmountPage, "页,平均每天读",
  ##'             round(averageAmountPage, digits = 2),"页,总共",
  ##'             sum(pageAmount),"页")
  ##' str6<-paste("发表书评",NR_REVIEW,"篇,读书笔记",NR_NOTE,"篇")
  ###############################################
  str1<-paste(YEAR,"\u5171\u9605\u8bfb\u8fc7",n,
              "\u672c\u4e66,\u603b\u4ef7\u503c",priceTotal,"\uffe5")
  str2<-paste("\u4e94\u661f\u597d\u4e66",star5num,"\u672c")
  str3<-paste(maxMonth,"\u6708\u6700\u52e4\u5feb\u8bfb\u4e86",maxAmount,"\u672c")
  str4<-paste("\u5e73\u5747",round(averageAmount,digits=2),"\u5929\u8bfb\u4e00\u672c\u4e66")
  str5<-paste(maxMonthPage,"\u6708\u6700\u591a\u8bfb\u4e86",maxAmountPage,
              "\u9875,\u5e73\u5747\u6bcf\u5929\u8bfb",
              round(averageAmountPage, digits = 2),"\u9875,\u603b\u5171",
              sum(pageAmount),"\u9875")
  str6<-paste("\u53d1\u8868\u4e66\u8bc4",NR_REVIEW,
              "\u7bc7,\u8bfb\u4e66\u7b14\u8bb0",NR_NOTE,"\u7bc7")
  Encoding(str1)<-"UTF-8";Encoding(str2)<-"UTF-8";Encoding(str3)<-"UTF-8"
  Encoding(str4)<-"UTF-8";Encoding(str5)<-"UTF-8";Encoding(str6)<-"UTF-8"

  png('statSummary.png',width=720,height=540)
  op<-par(mar=c(0,0,0,0),bty="n",yaxt="n",xaxt="n")
  cols<-rainbow(50,start = 0.5)
  plot(c(0, 1), c(0, 1),xlab="",ylab="",type="n")
  rect(0, 0,   1, 1, col = c(NA,0),border = NA, lwd = 2)
  ##我的阅读统计 \u6211\u7684\u9605\u8bfb\u7edf\u8ba1
  text(0.5,0.95,"\u6211\u7684\u9605\u8bfb\u7edf\u8ba1",cex=4,col="blue",,font = 2)
  text(0.5,0.8,str1,cex=2,col=cols[1])
  text(0.5,0.7,str2,cex=2,col=cols[5])
  text(0.5,0.6,str3,cex=2,col=cols[10])
  text(0.5,0.5,str4,cex=2,col=cols[15])
  text(0.5,0.4,str5,cex=2,col=cols[20])
  if(NR_REVIEW+NR_NOTE>0){
    text(0.5,0.3,str6,cex=2,col=cols[25])
  }
  ##不积跬步,无以至千里\u4e0d\u79ef\u8dec\u6b65,\u65e0\u4ee5\u81f3\u5343\u91cc
  ##不积小流,无以成江海 \u4e0d\u79ef\u5c0f\u6d41,\u65e0\u4ee5\u6210\u6c5f\u6d77
  text(0.5,0.2,"\u4e0d\u79ef\u8dec\u6b65,\u65e0\u4ee5\u81f3\u5343\u91cc",cex=4,col=cols[30],font = 2)
  text(0.5,0.1,"\u4e0d\u79ef\u5c0f\u6d41,\u65e0\u4ee5\u6210\u6c5f\u6d77",
       cex=4,col=cols[30], font = 2)
  par(op)
  dev.off()
  rm(x)
}
####################################
##按月统计
##'  @x:collect_df$month
##statByMonth(collect_df$month)
statByMonth<-function(x){
  ## 根据月份统计阅读量
  cat(" #### \u6839\u636e\u6708\u4efd\u7edf\u8ba1\u9605\u8bfb\u91cf......\n")
  amount<-table(x)
  png('barByMonth.png',width=720,height=540)
  op<-par(mar=c(2.5,0,2,0))
  cols<-rainbow(5*length(amount),start = 0.5)[1:length(amount)]
  barplot(amount,space=0,ylim=c(0,max(amount)+1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount+0.3,labels=amount,col=cols,cex=2,font=2)
  ## 阅读书籍数量by月 \u9605\u8bfb\u4e66\u7c4d\u6570\u91cfby\u6708
  title(main=list("\u9605\u8bfb\u4e66\u7c4d\u6570\u91cfby\u6708",cex=2,col="blue"))
  par(op)
  dev.off()
  rm(x)
}
#####################################
## 按页数统计
##'  @x:collect_df
## statByPage(collect_df[,c("month","pages")])
statByPage<-function(x){
  ## 统计阅读页数
  cat(" #### \u7edf\u8ba1\u9605\u8bfb\u9875\u6570......\n")
  amount<-tapply(as.integer(x$pages),x$month,sum,na.rm = T)
  amount2<-amount/max(amount)
  png('barByPage.png',width=720,height=540)
  op<-par(mar=c(2.5,0,3,0))
  cols<-topo.colors(5*length(amount),alpha=0.5)[1:length(amount)]
  barplot(amount2,space=0,ylim=c(0,1.1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount2+0.05,labels=amount,col=cols,cex=1.5,font=2)
  ## 阅读书籍页数by月
  title(main=list("\u9605\u8bfb\u4e66\u7c4d\u9875\u6570by\u6708",cex=2,col="blue"))
  par(op)
  dev.off()
  rm(x)
}

#############################################
##' 将df转化为DocumentTermMatrix
.df2dtm<-function(df,content='word',word.min=2,type='book'){
  if(!require(tm)){
    install.packages("tm")
    require(tm)
  }
  if(type=='book'){
    df <- data.frame(contents = as.character(df[,content]), 
                     id = as.character(df$bookid), 
                     heading = as.character(df$author),
                     origin=as.character(df$title),
                     stringsAsFactors = F)
  }
  else if(type=='movie'){
    df <- data.frame(contents = as.character(df[,content]), 
                     id = as.character(df$movieid), 
                     heading = as.character(df$author),
                     origin=as.character(df$title),
                     stringsAsFactors = F)
  }

  m <- list(Content = "contents", Heading = "heading",
            ID = "origin",Origin="id")
  myReader <- readTabular(mapping = m, language = "Zh_cn")
  corpus <- Corpus(DataframeSource(df), 
                   readerControl = list(reader = myReader,language = "Zh_cn"))
  dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(word.min, Inf)))
  rowTotals <- apply(dtm , 1, sum) 
  dtm <- dtm[rowTotals> 0]
  return(dtm)
}
###################################################
##' 根据标签的TF进行层次聚类
##' clusterCloudByTags(x=qxde$collect_df,k=8)
clusterCloudByTags<-function(x,k=8,max.words=100){
  if(!require(wordcloud)){
    install.packages("wordcloud")
    require(wordcloud)
  }
  tagsDtm<-.df2dtm(df=x,content="user_tags",word.min=1)
  ## tagsDtm<-weightTfIdf(tagsDtm)
  ##cat(" #### 根据我的标签对书籍进行层次聚类......\n")
  cat(" #### \u6839\u636e\u6211\u7684\u6807\u7b7e\u5bf9\u4e66\u7c4d\u8fdb\u884c\u5c42\u6b21\u805a\u7c7b......\n")
  diss<-dissimilarity(tagsDtm,method='cosine')
  hc <- hclust(diss, method = "ward")
  png('hclustByTags.png',width=720,height=540)
  op<-par(mar=c(0,0,3,0))
  if(k>nrow(x))k=nrow(x)-1
  ## 这些书自动归类 \u8fd9\u4e9b\u4e66\u81ea\u52a8\u5f52\u7c7b
  plot(hc,frame.plot=F,ann=T,hang=0.2,col=4,
       main=list('\u8fd9\u4e9b\u4e66\u81ea\u52a8\u5f52\u7c7b',
                 cex=2,col="blue"),ylab='',xlab='',sub='')
  if(k>1){
    rect.hclust(hc,k, border = rainbow(k))
  }
  
  par(op)
  dev.off()
  
  tagsmat<-as.matrix(tagsDtm)
  freq<-colSums(tagsmat)
  word<-colnames(tagsDtm)
  word<-gsub("<U\\+30FB>","",word)
  df<-data.frame(word=word,freq=freq)
  df<-df[order(df$freq,decreasing=T),]
  row.names(df)<-NULL
  ##cat(" #### 对我的标签绘制wordcloud......\n")
  cat(" #### \u5bf9\u6211\u7684\u6807\u7b7e\u7ed8\u5236wordcloud......\n")
  png('wordcloudByTags.png',width=720,height=720)
  wordcloud(words=df$word,
            freq=df$freq,
            min.freq=1,scale=c(5,1.4),
            max.words=max.words,
            random.order=F,
            ordered.colors=F,
            colors=topo.colors(length(table(df$freq))))
  ## 我的口味\u6211\u7684\u53e3\u5473
  title(main=list('\u6211\u7684\u53e3\u5473',cex=2,col="blue",font = 2))
  dev.off()
  rm(x,df,diss,tagsDtm)
}
#########################################
##' 将数据转化为关系形式
##'  @x:collect_df[,c("title","user_tags")]
graphByTag<-function(x){
  if(!require(igraph)){
    install.packages("igraph")
    require(igraph)
  }
  ##cat(" #### 绘制书籍标签之间的关系图.....\n")
  cat(" #### \u7ed8\u5236\u4e66\u7c4d\u6807\u7b7e\u4e4b\u95f4\u7684\u5173\u7cfb\u56fe.....\n")
  .todf<-function(x){
    x<-as.character(x)
    ## 标签:\u6807\u7b7e:
    taggg<-'\u6807\u7b7e:';Encoding(taggg)<-"UTF-8"
    x<-gsub(taggg,'',c(x[1],x[2]))
    left=x[1]
    right<-unlist(strsplit(x[2],' '))
    right<-right[nchar(right)>0]
    right<-right[right!=left]
    left<-rep(left,length(right))
    cbind(left,right)
  }
  tag_list<-apply(x[,c("title","user_tags")],1,.todf)
  n=length(tag_list)
  tag<-c()
  for(i in 1:n){
    tag<-rbind(tag,tag_list[[i]])
  }
  g<-graph.data.frame(tag,directed=F)
  title<-unique(x$title)
  col=rep(2,length(V(g)$name))
  col[match(V(g)$name,title)]<-4
  vcex=col;vcex[vcex==4]=1.2;vcex[vcex==2]=1
  png('graphByTag.png',width=720,height=720)
  plot(g, layout=layout.fruchterman.reingold, vertex.size=col,
       vertex.label.dist=0, vertex.color=col+1, 
       edge.arrow.size=0.5,vertex.label=V(g)$name,vertex.label.cex=vcex,
       vertex.label.color=col+2)
  ## 书籍与标签之间的关系图
  title(main=list("\u4e66\u7c4d\u4e0e\u6807\u7b7e\u4e4b\u95f4\u7684\u5173\u7cfb\u56fe",
                  font=2,col="blue"))
  dev.off()
  rm(x,tag,g)
}

#######################################
##' 分词函数
.f_seg<-function(x,stopwords=NULL){
  if(!require(Rwordseg)){
    install.packages("Rwordseg", repos = "http://R-Forge.R-project.org", type = "source")
    require(Rwordseg)
  }
  seg<-segmentCN(x)
  if(!is.null(stopwords)){
    seg<-seg[!seg %in% stopwords]
  }
  seg<-seg[nchar(seg)>1]
  word<-paste(seg,collapse=' ')
  word
}

#########################################
##' 对评论进行分词，绘制wordcloud
##' wordcloudByComment(x=qxde$collect_df)
## load("stopwords.rda")
wordcloudByComment<-function(x,max.words=100,stopwords,filename='wordcloudByComment'){
  if(!require(wordcloud)){
    install.packages("wordcloud")
    require(wordcloud)
  }
  ##cat(" #### 对评论数据进行分词......\n")
  cat(" #### \u5bf9\u8bc4\u8bba\u6570\u636e\u8fdb\u884c\u5206\u8bcd......\n")
  x<-x[!is.na(x$comment),]
  word<-sapply(x$comment,.f_seg,stopwords)
  names(word)<-NULL
  word<-unlist(strsplit(word," "))
  tmp<-sort(table(word),decreasing=T)
  df<-data.frame(word=names(tmp),freq=tmp[])
  #gsub(':','-',substr(Sys.time(),12,20))
  png(paste0(filename,'.png',sep=''),width=720,height=720)
  #par(bg='gray90')
  ##cat(" #### 对评论关键词绘制wordcloud......\n")
  cat(" #### \u5bf9\u8bc4\u8bba\u5173\u952e\u8bcd\u7ed8\u5236wordcloud......\n")
  wordcloud(words=df$word,
            freq=df$freq,
            min.freq=1,scale=c(5,1.4),
            max.words=max.words,
            random.order=F,
            ordered.colors=F,
            colors=rainbow(length(table(df$freq))))
  ## 我的评论关键词\u6211\u7684\u8bc4\u8bba\u5173\u952e\u8bcd 
  title(main=list('\u6211\u7684\u5410\u69fd',
                  cex=2,col="blue",font = 2))
  dev.off()
  rm(x,df,word)
}
#######################################################
##阅读统计可视化函数
##'  @x:user_book_status的输出
##'  @YEAR:统计年份
##' load("qxde.rda")
##' data(stopwords)
##' user_book_viz(x=qxde,YEAR="2012",stopwords=stopwords,back=TRUE)
user_book_viz<-function(x,YEAR="2013",stopwords=stopwords,back=FALSE){
  ## 正在预处理数据
  cat("\u6b63\u5728\u9884\u5904\u7406\u6570\u636e......\n")
  collect_df<-x$collect_df
  collect_df$reading_date<-gsub("[\n \u8bfb\u8fc7\u60f3\u5728]","",collect_df$reading_date)
  collect_df$price<-gsub("\u5143","",collect_df$price)
  ## 标签: \u6807\u7b7e:
  collect_df$user_tags<-gsub("\u6807\u7b7e: ","",collect_df$user_tags)
  year<-substr(collect_df$reading_date,1,4)
  month<-substr(collect_df$reading_date,1,7)
  collect_df<-data.frame(collect_df,
                         month=month,year=year,stringsAsFactors=F)
  reviews<-x$reviews
  notes<-x$notes
  collect_images<-x$collect_images
  if(!is.null(YEAR)){
    collect_df<-collect_df[year==YEAR,]
    collect_images<-collect_images[year==YEAR]
    r_year<-substr(reviews$published,1,4)
    n_year<-substr(notes$published,1,4)
    reviews<-reviews[r_year==YEAR,]
    notes<-notes[n_year==YEAR,]
  }
  NR_NOTE<-nrow(notes)
  NR_REVIEW=nrow(reviews)
  
  ## 拼图大小
  n<-length(collect_images)^0.5
  n1<-ceiling(n);n2=n1
  if((n1-1)^2+floor(n)>n^2){n2=floor(n)}
  require(EBImage)
  cat("\u6b63\u5728\u7ed8\u5236\u7edf\u8ba1\u56fe......\n") ##正在绘制统计图
  ##cat(" #### 绘制书籍封面拼图......\n")
  cat(" #### \u7ed8\u5236\u4e66\u7c4d\u5c01\u9762\u62fc\u56fe......\n")
  front<-combine(x=collect_images)
  png("front.png",width=64*n1,height=80*n2)
  display(x=front,method="raster",all=T)
  dev.off()
  ##基本统计
  statSummary(x=collect_df,YEAR=YEAR,NR_REVIEW,NR_NOTE)
  ##按月统计
  statByMonth(x=collect_df$month)
  ##按页数统计
  statByPage(x=collect_df[,c("month","pages")])
  ## 层次聚类与wordcloud
  clusterCloudByTags(x=collect_df,k=8)
  ##绘制书籍标签之间的关系图
  graphByTag(x=collect_df[,c("title","user_tags")])
  ## 评论关键词wordcloud 
  wordcloudByComment(x=collect_df,stopwords=stopwords)
  stat_base<-readImage("statSummary.png");h_base<-ncol(stat_base)
  stat_month<-readImage("barByMonth.png");h_month<-ncol(stat_month)
  stat_page<-readImage("barByPage.png");h_page<-ncol(stat_page)
  tag_clust<-readImage("hclustByTags.png");h_clust<-ncol(tag_clust)
  tag_cloud<-readImage("wordcloudByTags.png");h_tagcloud<-ncol(tag_cloud)
  tag_graph<-readImage('graphByTag.png');h_graph<-ncol(tag_graph)
  comment_cloud<-readImage("wordcloudByComment.png");h_cmt<-ncol(comment_cloud)
  im_front<-readImage("front.png")
  im_front<-resize(im_front,720,860);h_front<-ncol(im_front)
  #bigImage<-array(dim=c(720,h1+h2+h3+h4+h5+h6+h7+h8,3))
  bigImage<-array(dim=c(720,h_base+h_month+h_page+h_clust+
                          h_tagcloud,3))
  ### 添加背景色
  if(back==TRUE){
    ## 添加背景色
    cat("\u6dfb\u52a0\u80cc\u666f\u8272......\n")
    plum = readImage(system.file("images", "plum.jpg", package="Rdouban"))
     plum<-resize(plum,720,h_base)
     stat_base<-0.8*stat_base+0.2*plum
     writeImage(stat_base,"statSummary2.png") 
      
     orchid<-readImage(system.file("images", "orchid.jpg", package="Rdouban"))
     orchid<-resize(orchid,720,h_month)
     stat_month<-0.8*stat_month+0.2*orchid
     writeImage(stat_month,"stat_month2.png") 
     ######
     peony<-readImage(system.file("images", "peony.jpg", package="Rdouban"))
     peony<-resize(peony,720,h_page)
     stat_page<-0.9*stat_page+0.1*peony
     writeImage(stat_page,"stat_page2.png") 
      
     peach<-readImage(system.file("images", "peach.jpg", package="Rdouban"))
     peach<-resize(peach,720,h_clust)
     tag_clust<-0.8*tag_clust+0.2*peach
     writeImage(tag_clust,"tag_clust2.png") 
      
     #bamboo<-readImage(system.file("images", "bamboo.jpg", package="Rdouban"))
     im_front<-resize(im_front,720,h_front)
     #im_front<-im_front*0.8+0.2*bamboo
     #writeImage(im_front,"front2.png")
    
    fan<-readImage(system.file("images", "fan.jpg", package="Rdouban"))
    fan<-resize(fan,720,h_graph)
    tag_graph<-0.8*tag_graph+0.2*fan
    writeImage(tag_graph,"graphByTag2.png")
    
    rm(plum,orchid,peony,peach,fan)
    gc()
  }
  cat("\u5408\u5e76\u4e3a\u5927\u56fe......\n")##合并为大图
  #h_base+h_month+h_page+h_clust+h_tagcloud
  bigImage<-as.Image(bigImage);colorMode(bigImage)<-2
  bigImage[,1:h_base,]<-stat_base
  bigImage[,(h_base+1):(h_month+h_base),]<-stat_month
  bigImage[,(h_base+h_month+1):(h_page+h_month+h_base),]<-stat_page
  bigImage[,(h_base+h_month+h_page+1):(h_clust+h_page+h_month+h_base),]<-tag_clust
  bigImage[,(h_base+h_month+h_page+h_clust+1):(h_tagcloud+h_clust+h_page+h_month+h_base),]<-tag_cloud
  #bigImage[,(h1+h2+h3+h4+h5+1):(h6+h5+h4+h3+h2+h1),]<-tag_graph
  #bigImage[,(h1+h2+h3+h4+h5+h6+1):(h7+h6+h5+h4+h3+h2+h1),]<-comment_cloud
  #bigImage[,(h1+h2+h3+h4+h5+h6+h7+1):(h8+h7+h6+h5+h4+h3+h2+h1),]<-im_front
  rm(stat_base,stat_month,stat_page,tag_cloud,tag_clust)
  gc()
  writeImage(bigImage, files="bigImage.png", quality = 85)
  rm(bigImage)
  bigImage2<-array(dim=c(720,h_cmt+h_graph+h_front,3))
  bigImage2<-as.Image(bigImage2);colorMode(bigImage2)<-2
  bigImage2[,1:h_cmt,]<-comment_cloud
  bigImage2[,(1+h_cmt):(h_cmt+h_graph),]<-tag_graph
  bigImage2[,(1+h_cmt+h_graph):(h_cmt+h_graph+h_front),]<-im_front
  writeImage(bigImage2, files="bigImage2.png", quality = 85)
  rm(tag_graph,comment_cloud,im_front,bigImage2)
  ## 信息图存放位置：\u4fe1\u606f\u56fe\u5b58\u653e\u4f4d\u7f6e
  cat("\u4fe1\u606f\u56fe\u5b58\u653e\u4f4d\u7f6e:",getwd(),"\n")
  cat("\u751f\u6210\u7684\u56fe\u7247\u4e3a:\n",
      dir(getwd(),"png"),"\n") #生成的图片为:
}
