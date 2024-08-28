### 安装

```
remotes::install_git("https://gitee.com/anhuikylin/ait.git")
```
### 启动


```
Ait::run_Ait()
```
### 上传本地代码

```
git config --global user.name "anhuikylin"
git config --global user.email "1264284076@qq.com"

git config user.name
git config user.email

git init

git add .

git commit -m ait

#然后输入下列命令（xxx为刚才复制的仓库链接）

# git remote add origin xxx

git remote add origin https://gitee.com/anhuikylin/ait.git
git pull --rebase origin master
git push -u origin master
```
### 上传本地代码失败的其他方法

```
解决方法：
方法一、同步

1、git pull origin master --allow-unrelated-histories //把远程仓库和本地同步，消除差异
2、重新add和commit相应文件
3、git push origin master
4、此时就能够上传成功了
如果只是因为本地没有ReadMe文件，那么就在本地生成一个

git pull --rebase origin master  //本地生成ReadMe文件
git push origin master
方法二：强推

即利用强覆盖方式用你本地的代码替代git仓库内的内容

git push -f origin master
该命令会强制上传覆盖远程文件，慎用
方法三、

先把git的东西fetch到你本地然后merge后再push

git fetch
git merge
```
### R包开发记录

```
rm(list = ls())
library(readxl)
library(tidyverse)
maize_t2g.kegg <- read_excel("D:/R项目/B73_xyl-1_xyl-2蛋白组/B73_xyl-1_xyl-2_Proteome/data/B73_xyl-1_xyl-2_Proteome_KEGG_background.xlsx",
                       sheet = 1)
usethis::use_data(maize_t2g.kegg, internal = TRUE)
usethis::use_data(maize_t2g.kegg)
maize_t2n.kegg<- read_excel("D:/R项目/B73_xyl-1_xyl-2蛋白组/B73_xyl-1_xyl-2_Proteome/data/B73_xyl-1_xyl-2_Proteome_KEGG_background.xlsx",
                      sheet = 2)
usethis::use_data(maize_t2n.kegg)

maize_t2g.go <- read_excel("D:/R项目/B73_xyl-1_xyl-2蛋白组/B73_xyl-1_xyl-2_Proteome/data/GO_Enrichment.xlsx",
                             sheet = 1)
usethis::use_data(maize_t2g.go)
maize_t2n.go<- read_excel("D:/R项目/B73_xyl-1_xyl-2蛋白组/B73_xyl-1_xyl-2_Proteome/data/GO_Enrichment.xlsx",
                            sheet = 2)
usethis::use_data(maize_t2n.go)

gene_matrix <- read.delim("D:/R项目/B73_xyl-1_xyl-2蛋白组/B73_xyl-1_xyl-2_Proteome/result/data_clean.csv", sep = ",")
maize_gene_list <- gene_matrix %>% 
  select(X) %>% 
  rename(gene = X)
usethis::use_data(maize_gene_list)

# https://combine-australia.github.io/r-pkg-dev/checking-you-package.html

library('devtools')
# 填写DESCRIPTION
# 删除NAMESPACE和原始的R文件 man里面的hello
load_all()
document()
use_mit_license()
# use_testthat()
roxygen2::roxygenise()
check()

install()

build()

```