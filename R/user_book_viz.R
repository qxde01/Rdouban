##source("user_book_viz.R",encoding="UTF-8")
## load("stopwords.rda")
#阅读综合统计
## @x:collect_df
## @YEAR:统计年份
## @NR_REVIEW:书评篇数
## @NR_NOTE:笔记篇数
#statSummary(x=collect_df2)
statSummary<-function(x,YEAR=NULL,NR_REVIEW=0,NR_NOTE=0){
  cat(" #### 阅读综合统计......\n")
  n=nrow(x) #number
  star5num<-nrow(x[x$user_rating=="5",]) # star5 number
  amount<-table(x$month)
  maxMonth<-names(amount[amount==max(amount)])
  maxAmount<-max(amount)
  averageAmount<-(length(amount)*30.42)/sum(amount)
  priceTotal<-sum(as.double(x$price),na.rm = T)
  pageAmount<-tapply(as.integer(x$pages),x$month,sum,na.rm = T)
  maxMonthPage<-names(pageAmount[pageAmount==max(pageAmount)])
  maxAmountPage<-max(pageAmount)
  averageAmountPage<-sum(pageAmount)/(length(pageAmount)*30.42)
  str1<-paste(YEAR,"共阅读过",n,"书,总价值",priceTotal,"￥")
  str2<-paste("五星好书",star5num,"本")
  str3<-paste(maxMonth,"月最勤快读了",maxAmount,"本")
  str4<-paste("平均",round(averageAmount,digits=2),"天读一本书")
  str5<-paste(maxMonthPage,"月最多读了",maxAmountPage, "页,平均每天读",
              round(averageAmountPage, digits = 2),"页,总共",
              sum(pageAmount),"页")
  str6<-paste("发表书评",NR_REVIEW,"篇,读书笔记",NR_NOTE,"篇")
  png('statSummary.png',width=720,height=540)
  op<-par(mar=c(0,0,0,0),bty="n",yaxt="n",xaxt="n")
  cols<-rainbow(50,start = 0.5)
  plot(c(0, 1), c(0, 1),xlab="",ylab="",type="n")
  rect(0, 0,   1, 1, col = c(NA,0),border = NA, lwd = 2)
  text(0.5,0.95,"我的阅读统计",cex=4,col="blue",,font = 2)
  text(0.5,0.8,str1,cex=2,col=cols[1])
  text(0.5,0.7,str2,cex=2,col=cols[5])
  text(0.5,0.6,str3,cex=2,col=cols[10])
  text(0.5,0.5,str4,cex=2,col=cols[15])
  text(0.5,0.4,str5,cex=2,col=cols[20])
  if(NR_REVIEW+NR_NOTE>0){
    text(0.5,0.3,str5,cex=2,col=cols[25])
  }
  text(0.5,0.2,"不积跬步,无以至千里",cex=4,col=cols[30],font = 2)
  text(0.5,0.1,"不积小流,无以成江海",cex=4,col=cols[30], font = 2)
  par(op)
  dev.off()
}
####################################
##按月统计
## @x:collect_df$month
##statByMonth(collect_df$month)
statByMonth<-function(x){
  cat(" #### 根据月份统计阅读量......\n")
  amount<-table(x)
  png('barByMonth.png',width=720,height=540)
  op<-par(mar=c(2.5,0,2,0))
  cols<-rainbow(5*length(amount),start = 0.5)[1:length(amount)]
  barplot(amount,space=0,ylim=c(0,max(amount)+1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount+0.3,labels=amount,col=cols,cex=2,font=2)
  title(main=list("阅读书籍数量by月",cex=2,col="blue"))
  par(op)
  dev.off()
}
#####################################
## 按页数统计
## @x:collect_df
## statByPage(collect_df[,c("month","pages")])
statByPage<-function(x){
  cat(" #### 统计阅读页数......\n")
  amount<-tapply(as.integer(x$pages),x$month,sum,na.rm = T)
  amount2<-amount/max(amount)
  png('barByPage.png',width=720,height=540)
  op<-par(mar=c(2.5,0,3,0))
  cols<-topo.colors(5*length(amount),alpha=0.5)[1:length(amount)]
  barplot(amount2,space=0,ylim=c(0,1.1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount2+0.05,labels=amount,col=cols,cex=1.5,font=2)
  title(main=list("阅读书籍页数by月",cex=2,col="blue"))
  par(op)
  dev.off()
}

#############################################
## 将df转化为DocumentTermMatrix
.df2dtm<-function(df,content='word',word.min=2){
  if(!require(tm)){
    install.packages("tm")
    require(tm)
  }
  df <- data.frame(contents = as.character(df[,content]), 
                   id = as.character(df$bookid), 
                   heading = as.character(df$author),
                   origin=as.character(df$title),
                   stringsAsFactors = F)
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
##根据标签的TF进行层次聚类
##clusterCloudByTags(x=qxde$collect_df,k=8)
clusterCloudByTags<-function(x,k=8,max.words=100){
  if(!require(wordcloud)){
    install.packages("wordcloud")
    require(wordcloud)
  }
  tagsDtm<-.df2dtm(df=x,content="user_tags",word.min=1)
  #tagsDtm<-weightTfIdf(tagsDtm)
  cat(" #### 根据我的标签对书籍进行层次聚类......\n")
  diss<-dissimilarity(tagsDtm,method='cosine')
  hc <- hclust(diss, method = "ward")
  png('hclustByTags.png',width=720,height=540)
  op<-par(mar=c(0,0,3,0))
  if(k>nrow(x))k=nrow(x)-1
  plot(hc,frame.plot=F,ann=T,hang=0.2,col=4,
       main=list('这些书自动归类',cex=2,col="blue"),ylab='',xlab='',sub='')
  rect.hclust(hc,k, border = rainbow(k))
  par(op)
  dev.off()
  
  tagsmat<-as.matrix(tagsDtm)
  freq<-colSums(tagsmat)
  word<-colnames(tagsDtm)
  word<-gsub("<U\\+30FB>","",word)
  df<-data.frame(word=word,freq=freq)
  df<-df[order(df$freq,decreasing=T),]
  row.names(df)<-NULL
  cat(" #### 对我的标签绘制wordcloud......\n")
  png('wordcloudByTags.png',width=720,height=720)
  wordcloud(words=df$word,
            freq=df$freq,
            min.freq=1,scale=c(5,1.4),
            max.words=max.words,
            random.order=F,
            ordered.colors=F,
            colors=topo.colors(length(table(df$freq))))
  title(main=list('我的口味',cex=2,col="blue",font = 2))
  dev.off()
}
#########################################
#### 将数据转化为关系形式
## @x:collect_df[,c("title","user_tags")]
graphByTag<-function(x){
  if(!require(igraph)){
    install.packages("igraph")
    require(igraph)
  }
  cat(" #### 绘制书籍标签之间的关系图.....\n")
  .todf<-function(x){
    x<-as.character(x)
    x<-gsub('标签:','',c(x[1],x[2]))
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
  title(main=list("书籍与标签之间的关系图",font=2,col="blue"))
  dev.off()
}

#######################################
#### 分词函数
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
## 对评论进行分词，绘制wordcloud
##wordcloudByComment(x=qxde$collect_df)
## load("stopwords.rda")
wordcloudByComment<-function(x,max.words=100,stopwords){
  if(!require(wordcloud)){
    install.packages("wordcloud")
    require(wordcloud)
  }
  cat(" #### 对评论数据进行分词......\n")
  word<-sapply(x$comment,.f_seg,stopwords)
  names(word)<-NULL
  word<-unlist(strsplit(word," "))
  tmp<-sort(table(word),decreasing=T)
  df<-data.frame(word=names(tmp),freq=tmp[])
  png('wordcloudByComment.png',width=720,height=720)
  #par(bg='gray90')
  cat(" #### 对评论关键词绘制wordcloud......\n")
  wordcloud(words=df$word,
            freq=df$freq,
            min.freq=1,scale=c(5,1.4),
            max.words=max.words,
            random.order=F,
            ordered.colors=F,
            colors=rainbow(length(table(df$freq))))
  title(main=list('我的评论关键词',cex=2,col="blue",font = 2))
  dev.off()
}
#######################################################
##阅读统计可视化函数
## @x:user_book_status的输出
## @YEAR:统计年份
#### load("qxde.rda")
## data(stopwords)
## source("user_book_viz.R",encoding="UTF-8")
##user_book_viz(x=qxde,YEAR="2012",stopwords=stopwords,back=TRUE)
user_book_viz<-function(x,YEAR="2013",stopwords=stopwords,back=FALSE){
  cat("正在预处理数据......\n")
  collect_df<-x$collect_df
  collect_df$reading_date<-gsub("[\n 读过想在]","",collect_df$reading_date)
  collect_df$price<-gsub("[元]","",collect_df$price)
  collect_df$user_tags<-gsub("标签: ","",collect_df$user_tags)
  year<-substr(collect_df$reading_date,1,4)
  month<-substr(collect_df$reading_date,1,7)
  collect_df<-data.frame(collect_df,
                         month=month,year=year,stringsAsFactors=F)
  reviews<-x$reviews
  notes<-x$notes
  collect_imags<-x$collect_imags
  if(!is.null(YEAR)){
    collect_df<-collect_df[year==YEAR,]
    collect_imags<-collect_imags[year==YEAR]
    r_year<-substr(reviews$published,1,4)
    n_year<-substr(notes$published,1,4)
    reviews<-reviews[r_year==YEAR,]
    notes<-notes[n_year==YEAR,]
  }
  NR_NOTE<-nrow(notes)
  NR_REVIEW=nrow(reviews)
  
  ## 拼图大小
  n<-length(collect_imags)^0.5
  n1<-ceiling(n);n2=n1
  if((n1-1)^2+floor(n)>n^2){n2=floor(n)}
  library(EBImage)
  cat("正在绘制统计图......\n")
  cat(" #### 绘制书籍封面拼图......\n")
  front<-combine(collect_imags)
  png("front.png",width=64*n1,height=80*n2)
  display(front,method="raster",all=T)
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
  stat_base<-readImage("statSummary.png");h1<-ncol(stat_base)
  stat_month<-readImage("barByMonth.png");h2<-ncol(stat_month)
  stat_page<-readImage("barByPage.png");h3<-ncol(stat_page)
  tag_clust<-readImage("hclustByTags.png");h4<-ncol(tag_clust)
  tag_cloud<-readImage("wordcloudByTags.png");h5<-ncol(tag_cloud)
  tag_graph<-readImage('graphByTag.png');h6<-ncol(tag_graph)
  comment_cloud<-readImage("wordcloudByComment.png");h7<-ncol(comment_cloud)
  im_front<-readImage("front.png")
  im_front<-resize(im_front,720,900);h8<-ncol(im_front)
  bigImage<-array(dim=c(720,h1+h2+h3+h4+h5+h6+h7+h8,3))
  ### 添加背景色
  if(back==TRUE){
    cat("添加背景色......\n")
   plum = readImage(system.file("images", "plum.jpg", package="Rdouban"))
   plum<-resize(plum,720,h1)
   stat_base<-0.8*stat_base+0.2*plum
   orchid<-readImage(system.file("images", "orchid.jpg", package="Rdouban"))
   orchid<-resize(orchid,720,h2)
   stat_month<-0.8*stat_month+0.2*orchid
   #display(stat_month,method="raster")
   peony<-readImage(system.file("images", "peony.jpg", package="Rdouban"))
   peony<-resize(peony,720,h3)
   stat_page<-0.9*stat_page+0.1*peony
   #display(stat_page,method="raster")
   peach<-readImage(system.file("images", "peach.jpg", package="Rdouban"))
   peach<-resize(peach,720,h4)
   tag_clust<-0.8*tag_clust+0.2*peach
   #display(comment_cloud,method="raster")
   bamboo<-readImage(system.file("images", "bamboo.jpg", package="Rdouban"))
   bamboo<-resize(bamboo,720,h8)
    im_front<-im_front*0.8+0.2*bamboo
    fan<-readImage(system.file("images", "fan.jpg", package="Rdouban"))
    fan<-resize(fan,720,h6)
    tag_graph<-0.8*tag_graph+0.2*fan
    rm(plum,orchid,peony,peach,bamboo,fan)
    gc()
  }
  cat("合并为大图......\n")
  
  bigImage<-as.Image(bigImage);colorMode(bigImage)<-2
  bigImage[,1:h1,]<-stat_base
  bigImage[,(h1+1):(h2+h1),]<-stat_month
  bigImage[,(h1+h2+1):(h3+h2+h1),]<-stat_page
  bigImage[,(h1+h2+h3+1):(h4+h3+h2+h1),]<-tag_clust
  bigImage[,(h1+h2+h3+h4+1):(h5+h4+h3+h2+h1),]<-tag_cloud
  bigImage[,(h1+h2+h3+h4+h5+1):(h6+h5+h4+h3+h2+h1),]<-tag_graph
  bigImage[,(h1+h2+h3+h4+h5+h6+1):(h7+h6+h5+h4+h3+h2+h1),]<-comment_cloud
  bigImage[,(h1+h2+h3+h4+h5+h6+h7+1):(h8+h7+h6+h5+h4+h3+h2+h1),]<-im_front
  writeImage(bigImage, files="bigImage.png", quality = 85)
  cat("信息图存放位置：",getwd(),"\n")
  cat("生成的图片为：\n  ",dir(getwd(),"png"),"\n")
}