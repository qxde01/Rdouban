\encoding{UTF-8}
\name{get_music_info}
\alias{get_music_info}
\title{获取豆瓣音乐的专辑信息
}
\description{
获取豆瓣音乐的专辑信息,包括歌手、专辑名称等。
}
\usage{get_music_info(musicid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{musicid}{豆瓣音乐专辑ID号 }
}
\value{
  \item{title}{音乐专辑名称}
  \item{author }{作者}
  \item{rating}{评分信息,\code{average}为总评分,\code{votes}为评分听众总数,\code{starx}为对应的评分听众所占总评分的比例 }
  \item{summary }{内容简介}
  \item{tags }{一个data.frame,常用标签，两列分别是标签名称和标签频数}
  \item{songs}{专辑所包括的歌曲名称}
  \item{href}{音乐专辑主页网址}
  \item{image }{封面图片网址}
  \item{listeners}{听众信息,
   \code{doings}正在听的听众数量,\code{collections}已听过的听众数量,
   \code{wishes}指想听的用户数量}
  \item{comments_total}{发表评论的听众数}
  \item{attribute }{专辑的其他信息,比如出版公司、出版时间等}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  <\email{qxde01@gmail.com}>
}
\examples{
## http://music.douban.com/subject/3843530/
\dontrun{music<-get_music_info(musicid='3843530')}
}
\keyword{music}
\keyword{douban}
