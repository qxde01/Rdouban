\encoding{UTF-8}
\name{get_music_info}
\alias{get_music_info}

\title{获取豆瓣音乐的专辑信息
}
\description{
获取豆瓣音乐的专辑信息,包括歌手、专辑名称等。
}
\usage{get_music_info(musicid,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{musicid}{豆瓣音乐专辑ID号 }

}

\value{
  \item{title}{音乐专辑名称 }
  \item{base_info}{ 专辑基本信息，包括歌手、出版社、发行时间等 }
  \item{track}{ 专辑所包括的歌曲名称}
  \item{music_intro}{专辑简介 }
  \item{labels_amount}{ 听众所用标签总数 }
  \item{labels }{常见标签，一个包含2列的data.frame:标签名称\code{labels_name}及对应的标签频率\code{labels_freq}}
  \item{rating}{评分信息,\code{score}为总评分,\code{votes_amount}为评分听众总数,\code{starx}为对应的评分听众所占总评分的比例 }
  \item{audience}{听众信息, \code{comments_amount}为发表评论的听众数,\code{doings}指正在听的听众数量,\code{collections}指已听过的听众数量,\code{wishes}指想听的用户数量}}
  
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  <\email{qxde01@@gmail.com}>
}


\examples{
## http://music.douban.com/subject/3843530/
\dontrun{get_music_info(musicid='3843530')}
}

\keyword{music}
\keyword{douban}
