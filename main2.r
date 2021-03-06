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
#-----p和beta的汇总
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
  
  po<-li_all[[1]][["popular"]]
#-----得到积分
  get_inte<-function(data_1){
    fun<-approxfun(data_1[,1],data_1[,2])
    return(integrate(fun, 1, 500,subdivisions = 3000)[[1]])
  }
  g_inte<-NULL
  for(i in seq(li_all)){
    k<-data.frame(c(1:500),li_all[[i]][["popular"]])
    g_inte<-c(g_inte,get_inte(k))
  }
#-----绘制图形
  ##使用的数据10，19
  k<-data.frame(time=c(1:100),popular=li_all[[10]][["popular"]][1:100])
  p<-ggplot(k)
  p<-p+aes(time,popular)
  p+geom_line(size=2)+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
#------ 汇总p，beta，g_inte
  g_inte_log<-log(g_inte)

  data_1<-data.frame(p=p_s,beta=beta_s,g_inte,g_inte_log)
#-----绘图
  pp<-ggplot(data_1,aes(beta,g_inte_log,group=p,colour=p))
  pp+geom_line()+ylab("popular(log)")+xlab("beta")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
  
  pp<-ggplot(data_1,aes(beta,g_inte,group=p,colour=p))
  pp+geom_line()+ylab("popular")+xlab("beta")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
  
  pp<-ggplot(data_1,aes(p,g_inte_log,group=beta,colour=beta))
  pp+geom_line()+ylab("popular(log())")+xlab("p")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
  
  pp<-ggplot(data_1,aes(p,g_inte,group=beta,colour=beta))
  pp+geom_line()+ylab("popular")+xlab("p")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))

  #----接受人数
  num<-NULL
  for(i in seq(li_all)){
    num<-c(num,li_all[[i]][500,"acc_number_all"])
  }
  data_1<-data.frame(p=p_s,beta=beta_s,g_inte,g_inte_log,num)
  
  pp<-ggplot(data_1,aes(beta,num,group=p,colour=p))
  pp+geom_line()+ylab("number")+xlab("beta")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
  
  pp<-ggplot(data_1,aes(p,num,group=beta,colour=beta))
  pp+geom_line()+ylab("number")+xlab("p")+theme(axis.title =element_text(size=24),axis.text =element_text(size=24,face="bold"))
  
    
    
    
    
    
    
    
    
    
    
 