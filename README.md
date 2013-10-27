Rdouban
=======
### 获取豆瓣网评论数据的R接口，基于RCurl和XML。
> #### 非官方接口
* get_book_info	获取豆瓣图书基本信息
* get_book_reviews	获取豆瓣图书的评论文章
* get_book_discussions	获取豆瓣图书的话题讨论内容
* get_book_notes	获取豆瓣图书的读书笔记
* get_movie_info	获取豆瓣影视的基本信息
* get_movie_reviews	获取豆瓣电影的长篇影评
* get_movie_comments	获取豆瓣电影的短评
* get_movie_discussions	获取关于某个豆瓣电影的话题讨论内容
* get_music_info	获取豆瓣音乐的专辑信息
* get_music_reviews	获取豆瓣音乐评论信息.
* get_music_discussions	获取豆瓣音乐专辑的话题讨论内容
* user_book_status 获取用户的读书信息
* user_book_viz 对用户的读书信息进行统计和可视化

> #### 下面的函数是上述部分函数的重写，部分采用了官方API
* get.book.info  豆瓣图书信息API V2
* get.movie.info  豆瓣电影信息API V2
* get.music.info  豆瓣音乐信息API V2
* get.book.review  豆瓣图书评论信息,部分采用API V1
* get.movie.review  豆瓣电影长篇评论,通过移动网页m.douban.com,高频率访问会被豆瓣阻止
* get.movie.comment  豆瓣电影短评,通过移动网页m.douban.com,高频率访问会被豆瓣阻止

>### 安装
```
library("devtools")
install_github("Rdouban","qxde01")
```
>### 例子
获取作者qxde01的阅读信息，并统计分析和可视化。
```
qxde<-user_book_status(userid="qxde01")
data(stopwords) ## 中文停止词
## 生成用户qxde01的2013阅读可视化图形
user_book_viz(x=qxde,YEAR="2013",stopwords=stopwords,back=TRUE)
``` 
>### windows下安装
在windows下的中文编码至今没有搞定，**坑爹的windows，坑娘的微软**，安装编译时会出错，主要是因为帮助用中文编写，在R/etc/Rprofile.site 添加`options(encoding="UTF-8")`,使用RStudio可以编译通过，查看帮助会有乱码，使用浏览器查看有部分是乱码（需要手动修改浏览器编码查看方式）。在Linux下没有任何问题。