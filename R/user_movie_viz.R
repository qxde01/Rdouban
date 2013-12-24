##' 观影综合统计
MovieStatSummary<-function(x,YEAR=NULL,NR_REVIEW=0){
  cat(" \u89c2\u5f71\u6982\u8981\u7edf\u8ba1 ......\n")
  n=nrow(x) #number
  star5num<-nrow(x[x$user_rating=="5",]) # star5 number
  amount<-table(x$month)
  maxMonth<-names(amount[amount==max(amount)])[1]
  maxAmount<-max(amount) ##观看最多的月份
  averageAmount<-n/(length(amount))##每月几部
  duration<-as.double(x$duration)
  ## 没有时长数据的用平均值代替
  durationTotal<-n*sum(duration,na.rm = T)/length(duration[!is.na(duration)])##总时长
  durationAmount<-tapply(as.integer(x$duration),x$month,sum,na.rm = T)
  maxMonthduration<-names(durationAmount[durationAmount==max(durationAmount)])[1]
  maxAmountduration<-max(durationAmount) ##时长最大
  averageAmountduration<-sum(durationAmount)/n ##每月平均观影时长

  str1<-paste(YEAR,"\u5171\u89c2\u770b\u7535\u5f71",n, 
              "\u90e8,\u5408\u8ba1\u65f6\u957f",
              round(durationTotal,digits=0),"\u5206\u949f")
  str2<-paste("\u4e94\u661f\u597d\u7535\u5f71",star5num,"\u90e8")
  str3<-paste(maxMonth,"\u6708\u6700\u591a\u89c2\u770b\u4e86",maxAmount,"\u90e8")
  str4<-paste("\u5e73\u5747\u6bcf\u6708\u89c2\u770b",round(averageAmount,digits=2),"\u90e8")
  str5<-paste(maxMonthduration,"\u6708\u6700\u591a\u89c2\u770b", 
              maxAmountduration,"\u5206\u949f,\u5e73\u5747\u6bcf\u6708\u89c2\u770b",
              round(averageAmountduration, digits = 2),"\u5206\u949f")
  str6<-paste("\u5171\u53d1\u8868\u5f71\u8bc4",NR_REVIEW, "\u7bc7")
  #Encoding(str1)<-"UTF-8";Encoding(str2)<-"UTF-8";Encoding(str3)<-"UTF-8"
  #Encoding(str4)<-"UTF-8";Encoding(str5)<-"UTF-8";Encoding(str6)<-"UTF-8"
  
  #png('MovieStatSummary.png',width=720,height=540)
  op<-par(mar=c(0,0,0,0),bty="n",yaxt="n",xaxt="n")
  cols<-rainbow(50,start = 0.5)
  plot(c(0, 1), c(0, 1),xlab="",ylab="",type="n")
  rect(0, 0,   1, 1, col = c(NA,0),border = NA, lwd = 2)
  ##我的观影统计 
  text(0.5,0.95,"\u6211\u7684\u89c2\u5f71\u7edf\u8ba1",cex=4,col="blue",,font = 2)
  text(0.5,0.8,str1,cex=2,col=cols[1])
  text(0.5,0.7,str2,cex=2,col=cols[9])
  text(0.5,0.6,str3,cex=2,col=cols[18])
  text(0.5,0.5,str4,cex=2,col=cols[27])
  text(0.5,0.4,str5,cex=2,col=cols[36])
  if(NR_REVIEW>0){
    text(0.5,0.3,str6,cex=2,col=cols[45])
  }
  ##生活不是电影,但电影是生活,是镜子
  ##梦想始于剧本,而终结于电影
  text(0.5,0.2,"\u751f\u6d3b\u4e0d\u662f\u7535\u5f71,\u4f46\u7535\u5f71\u662f\u751f\u6d3b,\u662f\u955c\u5b50",cex=4,col=cols[30],font = 2)
  text(0.5,0.1,"\u68a6\u60f3\u59cb\u4e8e\u5267\u672c,\u800c\u7ec8\u7ed3\u4e8e\u7535\u5f71",
       cex=4,col=cols[30], font = 2)
  par(op)
  #dev.off()
  rm(x)
}
####################################
##按月统计
##'  @x:collect_df$month
##statByMonth(collect_df$month)
MovieStatByMonth<-function(x){
  ## 根据月份统计观影数量
  cat(" \u6839\u636e\u6708\u4efd\u7edf\u8ba1\u89c2\u5f71\u6570\u91cf......\n")
  amount<-table(x)
  #png('barByMonthMovie.png',width=720,height=540)
  op<-par(mar=c(2.5,0,2,0))
  cols<-rainbow(5*length(amount),start = 0.5)[1:length(amount)]
  barplot(amount,space=0,ylim=c(0,max(amount)+1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount+0.3,labels=amount,col=cols,cex=2,font=2)
  ## 观影数量by月 
  title(main=list("\u89c2\u5f71\u6570\u91cfby\u6708",cex=2,col="blue"))
  par(op)
  #dev.off()
  rm(x)
}
#####################################
## 按时长统计
##'  @x:collect_df
## statByPage(collect_df[,c("month","pages")])
MovieStatByPage<-function(x){
  ## 统计观影时长by月
  cat(" \u6309\u6708\u7edf\u8ba1\u89c2\u5f71\u65f6\u957f......\n")
  aver=sum(x$duration,na.rm = T)/length(x$duration[!is.na(x$duration)])
  x$duration[is.na(x$duration)]<-aver
  amount<-tapply(as.integer(x$duration),x$month,sum,na.rm = T)
  amount2<-amount/max(amount)
  #png('barByPageMovie.png',width=720,height=540)
  op<-par(mar=c(2.5,0,3,0))
  cols<-topo.colors(5*length(amount),alpha=0.5)[1:length(amount)]
  barplot(amount2,space=0,ylim=c(0,1.1),col=cols,yaxt="n")
  text((1:length(amount))-0.5,amount2+0.05,labels=amount,col=cols,cex=1.5,font=2)
  title(main=list("\u89c2\u5f71\u65f6\u957fby\u6708(\u5206\u949f)",cex=2,col="blue"))
  par(op)
  #dev.off()
  rm(x)
}
###################################################
##' 根据标签的TF进行层次聚类
##' clusterCloudByTags(x=qxde$collect_df,k=8)
MovieclusterCloudByTags<-function(x,k=10,max.words=100){
  if(!require(wordcloud)){
    install.packages("wordcloud")
    require(wordcloud)
  }
  tagsDtm<-.df2dtm(df=x,content="tags",word.min=1,type='movie')

  cat(" \u6839\u636e\u7535\u5f71\u6807\u7b7e\u8fdb\u884c\u5c42\u6b21\u805a\u7c7b......\n")
  diss<-dissimilarity(tagsDtm,method='cosine')
  hc <- hclust(diss, method = "ward")
  #png('hclustByTagsMovie.png',width=720,height=540)
  op<-par(mar=c(0,0,3,0))
  if(k>nrow(x))k=nrow(x)-1
  ## 这些书自动归类 \u8fd9\u4e9b\u4e66\u81ea\u52a8\u5f52\u7c7b
  plot(hc,frame.plot=F,ann=T,hang=0.2,col=4,cex=2,
       main=list('\u7535\u5f71\u5f52\u7c7b',
                 cex=1.5,col="blue"),ylab='',xlab='',sub='')
  if(k>1){
    rect.hclust(hc,k, border = rainbow(k))
  }
  par(op)
  #dev.off()
  
  tagsmat<-as.matrix(tagsDtm)
  freq<-colSums(tagsmat)
  word<-colnames(tagsDtm)
  word<-gsub("<U\\+30FB>","",word)
  df<-data.frame(word=word,freq=freq)
  df<-df[order(df$freq,decreasing=T),]
  row.names(df)<-NULL
  ##cat(" #### 对我的标签绘制wordcloud......\n")
  cat(" \u5bf9\u7535\u5f71\u6807\u7b7e\u7ed8\u5236wordcloud......\n")
  #png('wordcloudByTagsMovie.png',width=720,height=720)
  wordcloud(words=df$word,
            freq=df$freq,
            min.freq=1,scale=c(5,1.4),
            max.words=max.words,
            random.order=F,
            ordered.colors=F,
            colors=topo.colors(length(table(df$freq))))
  ## 我的口味\u6211\u7684\u53e3\u5473
  title(main=list('\u6211\u7684\u53e3\u5473',cex=2,col="blue",font = 2))
  #dev.off()
  rm(x,df,diss,tagsDtm)
}
#########################################
##' 将数据转化为关系形式
##'  @x:collect_df[,c("title","user_tags")]
MovieGraphByTag<-function(x){
  if(!require(igraph)){
    install.packages("igraph")
    require(igraph)
  }
  ##cat(" #### 绘制电影标签之间的关系图.....\n")
  cat(" \u7ed8\u5236\u7535\u5f71\u6807\u7b7e\u4e4b\u95f4\u7684\u5173\u7cfb\u56fe.....\n")
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
  tag_list<-apply(x[,c("title","tags")],1,.todf)
  n=length(tag_list)
  tag<-c()
  for(i in 1:n){
    tag<-rbind(tag,tag_list[[i]])
  }
  g<-graph.data.frame(tag,directed=F)
  title<-unique(x$title)
  col=rep(2,length(V(g)$name))
  col[match(V(g)$name,title)]<-4
  vcex=col;vcex[vcex==4]=2;vcex[vcex==2]=1.5
 # png('MovieGraphByTag.png',width=720,height=720)
  plot(g, layout=layout.fruchterman.reingold, vertex.size=col,
       vertex.label.dist=0, vertex.color=col+1, 
       edge.arrow.size=0.5,vertex.label=V(g)$name,vertex.label.cex=vcex,
       vertex.label.color=col+2)
  ## 电影与标签之间的关系图
  title(main=list("\u7535\u5f71\u4e0e\u6807\u7b7e\u4e4b\u95f4\u7684\u5173\u7cfb\u56fe",
                  font=2,cex=2,col="blue"))
  #dev.off()
  rm(x,tag,g)
}
####################################
##
##collect_df[,c('author','cast')]
##导演和演员的合作关系
actor2df<-function(x){
  f_split<-function(x){
    left<-unlist(strsplit(as.character(x[1]),split=','))
    right<-unlist(strsplit(as.character(x[2]),','))
    n1=length(left)
    n2=length(right)
    df<-c()
    for(i in 1:n1){
      tmp<-cbind(left=rep(left[i],n2),right=right)
      df<-rbind(df,tmp)
    }
    df
  }
  x[,2]<-gsub('\u7279\u9080\u987e\u95ee\uff1a|\u5236\u7247\u4eba\uff1a|\u7279\u9080\u987e\u95ee\uff1a','',x[,2])
  x<-x[nchar(x[,2])>0,]
  x<-x[nchar(x[,1])>0,]
  n=nrow(x)
  df<-c()
  for (i in 1:n){
    tmp<-f_split(x[i,])
    #cat(i,'\n')
    df<-rbind(df,tmp)
  }
  df<-df[df[,1]!=df[,2],]
  unique(df)
}
##################################
#' @x:collect_df[,c('author','cast')]
#' actorGraph(x=collect_df[,c('author','cast')])
actorGraph<-function(x){
  cat(' \u5bfc\u6f14\u548c\u6f14\u5458\u7684\u5408\u4f5c\u5173\u7cfb.....\n')
  df<-actor2df(x)
  g<-graph.data.frame(df,directed=F)
  dirctor<-unique(df[,1])
  actor<-unique(df[,2])
  col=rep(2,length(V(g)$name))
  col[match(V(g)$name,dirctor)]<-4
  vcex=col;vcex[vcex==4]=1.2;vcex[vcex==2]=1
  png('actorGraphg.png',width=900,height=900)
  plot(g, layout=layout.fruchterman.reingold, vertex.size=col,
       vertex.label.dist=0, vertex.color=col+1, 
       edge.arrow.size=0.5,vertex.label=V(g)$name,vertex.label.cex=vcex,
       vertex.label.color=col+2)
  title(main=list("\u5bfc\u6f14\u548c\u6f14\u5458\u7684\u5408\u4f5c\u5173\u7cfb",
                  font=2,cex=2,col="blue"))
  dev.off()
}

#actorGraph(x)

#######################################################
##电影统计可视化函数
##'  @x:user_movie_status的输出
##'  @YEAR:统计年份
##' load("qxde.rda")
##' data(stopwords)
##' user_movie_viz(x=qxde,YEAR="2012",stopwords=stopwords)
user_movie_viz<-function(x,YEAR="2013",stopwords=stopwords){
  ## 正在预处理数据
  cat("\u6b63\u5728\u9884\u5904\u7406\u6570\u636e......\n")
  collect_df<-x$collect_df

  collect_df$duration<-as.integer(gsub('\\([^)]+\\)|[^0-9]','',collect_df$duration))
  year<-substr(collect_df$watching_date,1,4)
  month<-substr(collect_df$watching_date,1,7)
  collect_df<-data.frame(collect_df,
                         month=month,year=year,stringsAsFactors=F)
  reviews<-x$reviews
  collect_images<-x$collect_images
  if(!is.null(YEAR)){
    collect_df<-collect_df[year==YEAR,]
    collect_images<-collect_images[year==YEAR]
    r_year<-substr(reviews$published,1,4)
    reviews<-reviews[r_year==YEAR,]
  }
  NR_REVIEW=nrow(reviews)
  
  ## 拼图大小
  n<-length(collect_images)^0.5
  n1<-ceiling(n);n2=n1
  if((n1-1)^2+floor(n)>n^2){n2=floor(n)}
  require(EBImage)
  cat("\u6b63\u5728\u7ed8\u5236\u7edf\u8ba1\u56fe......\n") ##正在绘制统计图
  cat(" \u7ed8\u5236\u7535\u5f71\u6d77\u62a5\u62fc\u56fe......\n")
  front<-combine(x=collect_images)
  png("Moviefront.png",width=64*n1,height=80*n2)
  display(x=front,method="raster",all=T)
  dev.off()
  png("Movie.png",width=640,height=3600)
  op<-par(mfrow=c(6,1),mar=c(2,0,1.5,0))
  ##基本统计
  MovieStatSummary(x=collect_df,YEAR=YEAR,NR_REVIEW)
  ##按月统计
  MovieStatByMonth(x=collect_df$month)
  ##按页数统计
  MovieStatByPage(x=collect_df[,c("month","duration")])
  ## 层次聚类与wordcloud
  MovieclusterCloudByTags(x=collect_df,k=8)
  ##绘制书籍标签之间的关系图
  MovieGraphByTag(x=collect_df[,c("title","tags")])
  dev.off()
  ## 评论关键词wordcloud 
  xx<-collect_df[!is.na(collect_df$comment),]
  if (length(nrow(xx))>0){
    wordcloudByComment(x=xx,stopwords=stopwords,filename='MoviewordcloudByComment')
  }
  ## 导演和演员的合作关系
  actorGraph(x=collect_df[,c('author','cast')])
  ## 信息图存放位置：\u4fe1\u606f\u56fe\u5b58\u653e\u4f4d\u7f6e
  cat("\u4fe1\u606f\u56fe\u5b58\u653e\u4f4d\u7f6e:",getwd(),"\n")
  cat("\u751f\u6210\u7684\u56fe\u7247\u4e3a:\n",
      dir(getwd(),"png"),"\n") #生成的图片为:
}
