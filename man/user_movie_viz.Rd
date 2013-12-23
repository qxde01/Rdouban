\encoding{UTF-8}
\name{user_movie_viz}
\alias{user_movie_viz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
豆瓣用户观影统计及可视化函数
}
\description{
豆瓣用户观影统计及可视化函数,观影综合统计、标签云图、海报拼图等。
}
\usage{
user_movie_viz(x,YEAR="2013",stopwords=stopwords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{有函数\code{user_book_status}产生的数据}
  \item{YEAR}{年份，比如"2013",如果为NULL，则统计所有数据}
  \item{stopwords}{\code{Package:Rwordseg}分词时使用的中文停止词}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
产生以下图形：
\item{1.}{观影综合统计}
\item{2.}{每月统计(部,时长)}
\item{3.}{根据电影标签归类}
\item{4.}{电影标签云图}
\item{5.}{电影名称与标签之间的关系图}
\item{6.}{短评云图}
\item{7.}{海报拼图}
\item{8.}{导演和演员合作关系图}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
qxde01<\email{qxde01@gmail.com}>
}

\examples{
## http://movie.douban.com/people/qxde01
\dontrun{
   qxde<-user_movie_status(userid="qxde01")
   data(stopwords)
   user_movie_viz(x=qxde,YEAR=NULL,stopwords=stopwords)
 }
}

\keyword{ douban }
\keyword{ movie }