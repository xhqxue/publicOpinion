  rm(list=ls())
  library(igraph)
  library(ggplot2)
  source("functions.r")
  source("main.r")
  source("graph_made.r")
  p_need<-seq(from=0.2,to=3,by=.2)
  beta_need<-seq(from=0.2,to=1,by=.1)
  num_node<-1000
  run_time<-20
  li_all<-list()
  g_num<-0
  for(i in seq(p_need)){
    for(ii in seq(beta_need)){
      k1<-graph_node(num_node,p_set=p_need[i],beta_need=beta_need[ii])
      for(iii in seq(run_time)){
       if(iii==1){
         k2<-main(k1)/run_time
       }else{
         k2<-k2+main(k1)/run_time
       }
      }
      g_num<-g_num+1
      li_all[[g_num]]<-k2
      
    }
  }
 