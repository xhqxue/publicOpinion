  rm(list=ls())
  library(igraph)
  library(ggplot2)
  source("functions.r")
  source("main.r")
  source("graph_made.r")
  p_need<-seq(from=0.2,to=3,by=.2)
  beta_need<-seq(from=0.2,to=1,by=.1)
  num_node<-1000
  run_time<-50
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
  
  p_s<-NULL
  beta_s<-NULL
  for(i in seq(p_need)){
    for(ii in seq(beta_need)){
      p_s<-c(p_s,p_need[i])
      beta_s<-c(beta_s,beta_need[ii])
    }
  }
  acc_num<-NULL
  for(i in seq(li_all)){
    acc_num<-c(acc_num,li_all[[i]][500,"acc_number_all"])
  }
  data1<-data.frame(p_s,beta_s,acc_num)
  
  data_c<-function(lia,name){
    aa1<-NULL
    for(i in seq(li_all)){
      aa1<-c(aa1,lia[[i]][500,name])
    }
    return(aa1)
  }
  
  # data3<-data.frame(x1,x2,z)
  fit<-cusp(y~acc_num,alpha~p_s,beta~beta_s,data1)
  
  
  
  
 