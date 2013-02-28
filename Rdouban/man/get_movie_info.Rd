\encoding{UTF-8}
\name{get_movie_info}
\alias{get_movie_info}

\title{获取豆瓣影视的基本信息}
\description{获取豆瓣影视剧的基本信息,包括电影名称、导演、演员、豆瓣评分等信息.
}
\usage{
get_movie_info(movieid,...)
}
\arguments{
  \item{movieid}{豆瓣电影和电视剧的ID,由数字组成的字符串,可通过每部电影或电视剧的豆瓣主页查询.}
}
\value{一个列表(list), 包括:
  \item{movie_base_info}{豆瓣影视剧的基本信息,包括电影名称、导演、演员}
  \item{votes_info}{长度为7的向量,用户评分信息,score为总分,votes指参与的评分人数,starsx指对应评分用户数所占的比例}
  \item{movie_intro}{电影或电视剧简介}
  \item{labels_amount}{用户常用标签数量}\item{labels}{一个data.frame,\code{label_name}标签名称及对应的使用频率\code{label_freq}}\item{long_comment_votes}{长度为5向量,发表长影评的用户评分情况}
  \item{comments_amount}{长度为3向量,分别为短影评数量\code{short_comments},长影评数量\code{long_comments},讨论话题数量\code{discussion}}
\item{audience}{观众信息,长度为2的向量,\code{collections}指已观看的用户数量,\code{wishes}指想看的用户数量}
}
\author{<\email{qxde01@@gmail.com}>
}
\note{\code{movieid}必须是豆瓣电影或电视剧有效id,比如\link{http://movie.douban.com/subject/5308265/}的ID=5308265.
}

\examples{
\dontrun{get_movie_info(movieid='5308265')}
}

\keyword{movie}
