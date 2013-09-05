\encoding{UTF-8}
\name{get_music_discussions}
\alias{get_music_discussions}
\title{获取豆瓣音乐专辑的话题讨论内容
}
\description{获取豆瓣音乐专辑的话题讨论内容.
}
\usage{
get_music_discussions(musicid,results = 100, fresh = 10,count=20, verbose = TRUE,...)
}
\arguments{
  \item{musicid}{豆瓣音乐专辑的ID}
  \item{results}{获取话题讨论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{count}{每页discussion_uri数量,默认20，最大20}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\value{一个data.frame,各列为:
 \item{discussion_uri }{话题主页URL}
 \item{title }{话题标题题}
  \item{published }{话题发表的时间}
 \item{author}{作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{discussion }{话题详细内容}
  \item{useful }{被赞为有用的次数}
  \item{unuseful}{被评为无用的次数}
}

\author{
qxde01<\email{qxde01@gmail.com}>
}

\examples{
## http://music.douban.com/subject/3843530/discussion/
\dontrun{a<-get_music_discussions(musicid=3843530,results = 100)}
}
\keyword{discussion}
