library(vegan)



#########################
###
### Code for Alroy(2015) modification of the Forbers metric
### The argument "occs" should be a presence/absence matrix, 
### with taxa in rows, locality in columns
###
#########################



Alroy_Forbes<-function(occs)
{
  output<-matrix(nrow=ncol(occs),ncol=ncol(occs))
  rownames(output)<-colnames(occs)
  colnames(output)<-colnames(occs)
  for(i in 1:nrow(output))
  {
    for (j in 1:ncol(output))
    {
      species.A<-rownames(occs)[which(occs[,rownames(output)[i]]!=0)]
      species.B<-rownames(occs)[which(occs[,colnames(output)[j]]!=0)]
      
      n<-length(unique(c(species.A,species.B)))
      
      a<-length(intersect(species.A,species.B))
      b<-length(which(species.A%in%species.B==F))
      c<-length(which(species.B%in%species.A==F))
      
      output[i,j]<-1-((a*(n+sqrt(n)))/((a*(n+sqrt(n)))+(3/2*b*c)))
      
    }
  }
  return(as.dist(output))
}





#########################
###
### Code for RACB Diversity
### Arguments:
###
### data: Matrix with taxa in rows and locality in columns. 
### 	    Frequencies, not presence/absence
###
### metric: Four distance metrics available (code should be easy enough 
###		to add new ones). "Forbes", "Sorenson","Leonnon", "Bray"
###
### sim.iter: the number of simulated homogenous datasets to generate
###
### samp.stand: True or False, whether to include sampling standardisation
###
### samp.stand.size: Coverage to which each locality will be subsampled to. 
###			   Should be a number between 0 and 1. Note that if any 
###			   localities have a coverage of less than that specified
###			   (measured by Good's u), they will not be included in
###			   the calculation.
###
### samp.stand.iter: Number of size-standardised datasets to generate.
###
#########################





RACB.Diversity<-function(data,metric="Sorenson",sim.iter=1000,samp.stand=T,samp.stand.size=0.6,samp.stand.iter=1000)
{
  output<-vector(length=7)
  names(output)<-c("Raw Beta Diversity","Null Expectation","Null Upper","Null Lower","RAC Beta Diversity","RAC Upper","RAC Lower")
  freq<-data	
  
  if(samp.stand==F)
  {	
    test.data<-freq
    if(metric=="Forbes")
    {
      distances<-Alroy_Forbes(test.data)
    }	
    else if(metric=="Sorenson")
    {
      distances<-1-betadiver(t(test.data),method=11)
    }
    else if(metric=="Lennon")
    {
      distances<-betadiver(t(test.data),method=22)
    }
    else(distances<-vegdist(t(test.data),"bray"))
    
    output["Raw Beta Diversity"]<-mean(distances)
  }
  
  else
  {
    goods.u.vals<-vector(length=ncol(freq))
    for(i in 1:length(goods.u.vals))
    {
      singletons<-length(which(freq[,i]==1))
      specimens<-sum(freq[,i])
      goods.u.vals[i]<-1 - singletons / specimens
    }
    
    
    stand.beta.divs<-vector(length=samp.stand.iter)
    test.data<-freq[,which(goods.u.vals>=samp.stand.size)]
    
    for(i in 1:samp.stand.iter)
    {		
      
      sub.data<-matrix(nrow=nrow(test.data),ncol=ncol(test.data),data=0)
      rownames(sub.data)<-rownames(test.data)
      colnames(sub.data)<-colnames(test.data)
      
      for(j in 1:ncol(test.data))
      {
        
        
        samp.order<-sample(rep(rownames(test.data),test.data[,j]))
        u.seq<-vector(length=length(samp.order))
        for(k in 1:length(u.seq))
        {
          sub.test<-table(samp.order[1:k])
          specimens<-sum(sub.test)
          singletons<-length(which(sub.test==1))
          u.seq[k]<-1-singletons/specimens
        }
        
        drawn<-table(samp.order[1:(max(which(u.seq<samp.stand.size))+1)])
        sub.data[names(drawn),j]<-drawn
        
        
      }
      
      if(metric=="Forbes")
      {
        distances<-Alroy_Forbes(sub.data)
      }				
      else if(metric=="Sorenson")
      {
        distances<-1-betadiver(t(sub.data),method=11)
      }
      else if(metric=="Lennon")
      {
        distances<-betadiver(t(sub.data),method=22)
      }
      else(distances<-vegdist(t(sub.data),"bray"))
      
      stand.beta.divs[i]<-mean(distances)
    }
    output["Raw Beta Diversity"]<-mean(stand.beta.divs)
  }
  
  
  taxon.obs<-rowSums(freq)
  tot.tax<-sum(taxon.obs)
  taxon.frequencies<-taxon.obs/tot.tax
  locality.specimens<-colSums(freq)
  
  sim.res<-vector(length=sim.iter)
  for(i in 1:sim.iter)
  {
    sim.data<-matrix(nrow=nrow(freq),ncol=ncol(freq),data=0)
    rownames(sim.data)<-rownames(freq)
    colnames(sim.data)<-colnames(freq)
    for(j in 1:ncol(sim.data))
    {
      tax.sampled<-sample(names(taxon.frequencies),locality.specimens[j],replace=T,prob=taxon.frequencies)
      for(k in 1:length(tax.sampled))
      {
        sim.data[tax.sampled[k],j]<-sim.data[tax.sampled[k],j]+1
      }
    }
    
    
    
    if(samp.stand==F)
    {
      test.data<-sim.data
      if(metric=="Forbes")
      {
        distances<-Alroy_Forbes(test.data)
      }	
      else if(metric=="Sorenson")
      {
        distances<-1-betadiver(t(test.data),method=11)
      }
      else if(metric=="Lennon")
      {
        distances<-betadiver(t(test.data),method=22)
      }
      else(distances<-vegdist(t(test.data),"bray"))
      
      sim.res[i]<-mean(distances)
    }
    
    else
    {
      goods.u.vals<-vector(length=ncol(sim.data))
      for(j in 1:length(goods.u.vals))
      {
        singletons<-length(which(sim.data[,j]==1))
        specimens<-sum(freq[,j])
        goods.u.vals[j]<-1 - singletons / specimens
      }			
      
      
      stand.beta.divs<-vector(length=samp.stand.iter)
      test.data<-sim.data[,which(goods.u.vals>samp.stand.size)]
      
      for(j in 1:samp.stand.iter)
      {	
        sub.data<-matrix(nrow=nrow(test.data),ncol=ncol(test.data),data=0)
        rownames(sub.data)<-rownames(test.data)
        colnames(sub.data)<-colnames(test.data)
        
        for(k in 1:ncol(test.data))
        {
          
          samp.order<-sample(rep(rownames(test.data),test.data[,k]))
          u.seq<-vector(length=length(samp.order))
          for(m in 1:length(u.seq))
          {
            sub.test<-table(samp.order[1:m])
            specimens<-sum(sub.test)
            singletons<-length(which(sub.test==1))
            u.seq[m]<-1-singletons/specimens
          }
          
          drawn<-table(samp.order[1:(max(which(u.seq<samp.stand.size))+1)])
          sub.data[names(drawn),k]<-drawn
          
          
        }
        
        if(metric=="Forbes")
        {
          distances<-Alroy_Forbes(sub.data)
        }	
        else if(metric=="Sorenson")
        {
          distances<-1-betadiver(t(sub.data),method=11)
        }
        else if(metric=="Lennon")
        {
          distances<-betadiver(t(sub.data),method=22)
        }
        else(distances<-vegdist(t(sub.data),"bray"))
        
        stand.beta.divs[j]<-mean(distances)
      }
      sim.res[i]<-mean(stand.beta.divs)
    }
  }	
  output["Null Expectation"]<-mean(sim.res)
  upper<-0.95*sim.iter
  lower<-0.05*sim.iter
  
  output["Null Upper"]<-sort(sim.res)[upper]
  output["Null Lower"]<-sort(sim.res)[lower]
  
  
  dif.raw.sim<-output["Raw Beta Diversity"]-output["Null Expectation"]
  output["RAC Beta Diversity"]<-(dif.raw.sim)/((1-output["Null Expectation"]))
  
  dif.upper<-output["Raw Beta Diversity"]-output["Null Upper"]
  output["RAC Upper"]<-(dif.upper)/((1-output["Null Upper"]))
  
  dif.lower<-output["Raw Beta Diversity"]-output["Null Lower"]
  output["RAC Lower"]<-(dif.lower)/((1-output["Null Lower"]))
  
  
  
  return(output)
}


