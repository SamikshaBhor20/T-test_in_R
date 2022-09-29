# T-test_in_R
#A t-test is a statistical test that is used to compare the means of two groups. It is often used in hypothesis testing to determine whether a process or treatment             #actually has an effect on the population of interest, or whether two groups are different from one another.


#T-test
data_t<-readRDS('log2cpm_other.rds')                                 #Here we are using log2cpm file which we are allready save in .rds format & which will be getting                                                                               #from the normalization of matrix.

mat_data<- matrix(NA, ncol=4, nrow = nrow(data))
rownames(mat_data)<-rownames(data_t)

colnames(mat_data)<-c('test','control','pval','log2fc')

for(i in 1:nrow(data_t)){                                        #When it is row-wise, then we have to define it as an vector
  vec1 <- as.numeric(data_t[i, 1:4])
  vec2 <- as.numeric(data_t[i, 5:7])
  
  res <- t.test(vec1,vec2,paired=F, alternative='two-sided')
  mat[i,1]<-res$estimate[[1]]
  mat[i,2]<-res$estimate[[2]]
  mat[i,3]<-res$p.value
  mat[i,4]<-mat[i,1]-mat[i,2]
}
mat<-as.data.frame(mat)
num<-which(is.nan(mat$pval))
num

mat[num,'pval']<-1
View(mat)

library(EnhancedVolcano)

EnhancedVolcano(mat, lab=rownames(mat), x='log2FC', y='pval')
