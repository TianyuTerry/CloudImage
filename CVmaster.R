#group function
group_fun1=function(image0,folds=6,drop_margin=FALSE){
  ymin=2; ymax=383
  xmin=65;xmax=369
  group_image0=data.frame()
  x0=xmin
  while(x0<=xmax){
    y0=ymin
    while(y0<=ymax){
      temp_image0=image0%>%
        filter(y>=y0&y<=(y0+7)
               &x>=x0&x<=(x0+7)
               &label!=0)
      if(drop_margin){
        numrow=image0%>%
          filter(y>=y0&y<=(y0+7)
                 &x>=x0&x<=(x0+7))%>%
          nrow()
        label_sum=temp_image0%>%
          select(label)%>%
          sum()%>%
          abs()
        if(label_sum!=numrow){
          temp_image0=data.frame()
        }
      }
      if(nrow(temp_image0)!=0){
        temp_image0=temp_image0%>%
          mutate(fold=sample(1:folds,1))
        group_image0=rbind(group_image0,temp_image0)
      }
      y0=y0+8
    }
    x0=x0+8
    #print(x)
  }
  return(group_image0)
}
CVmaster=function(generic_fun="logistics",X,y,K,loss_fun="accuracy",drop_margin0=FALSE){
  
  CV_data=cbind(X_train,y_train)
  group_data=group_fun1(CV_data,drop_margin = drop_margin0)
  for(i in 1:(K-1)){
    train_data=group_data%>%filter(fold==i)
    if(generic_fun=="logistics"){
      clf=glm(label~.,train_data,family = "binomial")
      train_pred=
      test_pred=
        
    }
    if(generic_fun=="QDA"){
      clf=qda(label~.,train_data,family = "binomial")
    }
    
  }
  
}