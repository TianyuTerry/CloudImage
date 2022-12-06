#group function
group_fun1=function(image0,folds=6,drop_margin=FALSE){
  ymin=2; ymax=383
  xmin=65;xmax=369
  group_image0=data.frame()
  x0=xmin
  while(x0<=xmax){
    y0=ymin
    while(y0<=ymax){
      fold_num=sample(1:folds,1)
      temp_image0=image0%>%
        filter(y>=y0&y<=(y0+7)
               &x>=x0&x<=(x0+7)
               &label!=0)
      if(drop_margin&fold_num!=folds){
        numrow=image0%>%
          filter(y>=y0&y<=(y0+7)
                 &x>=x0&x<=(x0+7))%>%
          nrow()
        label_sum=temp_image0%>%
          dplyr::select(label)%>%
          sum()%>%
          abs()
        if(label_sum!=numrow){
          temp_image0=data.frame()
        }
      }
      if(nrow(temp_image0)!=0){
        temp_image0=temp_image0%>%
          mutate(fold=fold_num)
        group_image0=rbind(group_image0,temp_image0)
      }
      y0=y0+8
    }
    x0=x0+8
    #print(x)
  }
  return(group_image0)
}
# group pixels in blocks and assign group number for each image.
group_fun=function(img,size){
  ymin=min(img$y);ymax=max(img$y)
  xmin=min(img$x);xmax=max(img$x)
  group_image=data.frame()
  grp=1
  x=xmin
  while(x<=xmax){
    y=ymin
    while(y0<=ymax){
      block=image0%>%
        filter(y>=y0&y<=(y0+size-1)
               &x>=x0&x<=(x0+size-1)
               &label!=0)%>%
        mutate(group=grp)
      group_image=rbind(group_image,block)
      y0=y0+size
      grp=grp+1
    }
    x0=x0+size
  }
  return(group_image)
}
# combine images together and split into train/val/test
group_fun2=function(image0,folds=6){
  image1=image0%>%filter(label==1)
  image2=image0%>%filter(label==-1)
  n1=nrow(image1)
  n2=nrow(image2)
  group_list1=even_fold(n1,folds)
  group_list2=even_fold(n2,folds)
  image1=image1%>%mutate(fold=group_list1)
  image2=image2%>%mutate(fold=group_list2)
  return(rbind(image1,image2))
  
  
}
even_fold=function(num_row,k){
  seq_k=1:k
  nseq=ceiling(num_row/k)
  group_list=c()
  for(i in 1:nseq){
    group_list=c(group_list,seq_k)
  }
  group_list=head(group_list,num_row)
  group_list=group_list[sample(1:num_row,replace = FALSE)]
  return(group_list)
}


CVmaster=function(generic_fun="logistics",X,y,K,loss_fun="accuracy",drop_margin0=FALSE){
  train_model="factor(label)~NDAI+CORR+SD"
  CV_data=cbind(X,y)
  group_data=group_fun1(CV_data,drop_margin = drop_margin0)
  valid_acc=c()
  for(i in 1:(K-1)){
    train_data=group_data%>%filter(fold!=i&fold!=K)
    valid_data=group_data%>%filter(fold==i)
    y_valid=valid_data$label
    if(generic_fun=="logistics"){
      clf=glm(as.formula(train_model),train_data,family="binomial")
      valid_pred=predict(clf,valid_data,type="response")
      valid_pred=sign(valid_pred-0.5)
    }
    else if(generic_fun=="QDA"){
      clf=qda(formula=as.formula(train_model),data=train_data)
      valid_pred=as.numeric(as.character(predict(clf,valid_data)$class))
    }
    else if(generic_fun=="LDA"){
      clf=lda(formula=as.formula(train_model),data=train_data)
      valid_pred=as.numeric(as.character(predict(clf,valid_data)$class))
    }
    else if(generic_fun=="NB"){
      clf=naive_bayes(as.formula(train_model),data=train_data,usekernel=T) 
      valid_pred=sign(predict(clf,valid_data,type='prob')[,2]-0.5)
    }
    else if(generic_fun=="CART"){
      model=rpart(as.formula(train_model),data=train_data,method="class") 
      valid_pred=sign(predict(model,valid_data,type='prob')[,2]-0.5)
    }
    if("accuracy"%in%loss_fun){
      valid_acc=c(valid_acc,mean(valid_pred==y_valid))
    }
  }
  train_data=group_data%>%filter(fold!=K)
  test_data=group_data%>%filter(fold==K)
  y_test=as.numeric(test_data$label)
  if(generic_fun=="logistics"){
    clf=glm(as.formula(train_model),train_data,family="binomial")
    prob_pred=predict(clf,test_data,type="response")
    prob_train=predict(clf,train_data,type="response")
    test_pred=sign(prob_pred-0.5)
  }
  else if(generic_fun=="QDA"){
    clf=qda(formula=as.formula(train_model),data=train_data)
    clf_pred=predict(clf,test_data)
    test_pred=as.numeric(as.character(clf_pred$class))
    prob_pred=clf_pred$posterior[,2]
    prob_train=predict(clf,train_data)$posterior[,2]
  }
  else if(generic_fun=="LDA"){
    clf=lda(formula=as.formula(train_model),data=train_data)
    clf_pred=predict(clf,test_data)
    test_pred=as.numeric(as.character(clf_pred$class))
    prob_pred=clf_pred$posterior[,2]
    prob_train=predict(clf,train_data)$posterior[,2]
  }
  else if(generic_fun=="NB"){
    clf=naive_bayes(as.formula(train_model),data=train_data,usekernel=T) 
    prob_pred=predict(clf,test_data,type='prob')[,2]
    test_pred=sign(prob_pred-0.5)
    prob_train=predict(clf,train_data,type='prob')[,2]
  }
  else if(generic_fun=="CART"){
    clf=rpart(as.formula(train_model),data=train_data,method="class") 
    prob_pred=predict(clf,test_data,type='prob')[,2]
    test_pred=sign(prob_pred-0.5)
    prob_train=predict(clf,train_data,type='prob')[,2]
  }
  res=list(prob=prob_pred,
           gold=y_test,
           test=test_data,
           train=train_data,
           score=prob_train,
           model=clf)
  if("accuracy"%in%loss_fun){
    test_acc=mean(test_pred==y_test)
    res$acc=c(valid_acc,mean(valid_acc),test_acc)
  }
  if("prf"%in%loss_fun){
    precision=posPredValue(as.factor(test_pred),as.factor(y_test),positive=1)
    recall=sensitivity(as.factor(test_pred),as.factor(y_test),positive=1)
    f1=(2*precision*recall)/(precision+recall)
    res$prf=c(precision,recall,f1)
  }
  if("auc"%in%loss_fun){
    pred=prediction(prob_pred,y_test)
    perf=performance(pred,"auc")
    res$auc=slot(perf,"y.values")[[1]]
  }
  return(res)
}
k=6
nrow0=100
seq_k=1:k
nseq=ceiling(nrow0/k)
group_list=c()
for(i in 1:nseq){
  group_list=c(group_list,seq_k)
}
group_list=head(group_list,nrow0)
group_list[sample(1:nrow0)]
