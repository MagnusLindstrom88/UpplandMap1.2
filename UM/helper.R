round_df <- function(x, digits) {

  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

karta<- function(x,v){
  
  y<-switch(x,
            "Parker & grönområden" = c("Area","X1a","X1b","X1c","X1d","X1e"),
            "Mångfald i bostadsutbudet" =c("Area","X2a","X2b","X2c","X2d","X2e"),
            "Levandegöra gemensamma platser" =c("Area","X3a","X3b","X3c","X3d","X3e"),
            "Kommunikationer" = c("Area","X4a","X4b","X4c","X4d","X4e"),
            "Kultur & fritid" = c("Area","X5a","X5b","X5c","X5d","X5e"),
            "Utbildning" = c("Area","X6a","X6b","X6c","X6d","X6e"),
            "Omsorg" = c("Area","X7a","X7b","X7c","X7d","X7e"),
            "Skolan" = c("Area","X8a","X8b","X8c","X8d","X8e"),
            "Trygghet" = c("Area","X9a","X9b","X9c","X9d","X9e"),
            "Hållbar utveckling" = c("Area","X10a","X10b","X10c","X10d","X10e")
  )

   a<-subset(v,select=y)
   a<-internal_popup(x,a)
   a$favorit = names(a)[-1][apply(a[-1],1,which.max)]
   antalRader<-nrow(a) 
   if(!antalRader==43){
   b<- data6
   b3<-subset(b,select=y)
   part<-b3[!b3$Area %in% a$Area,]
   part<-internal_popup(x,part)
   part$favorit = NA
   a<-rbind(a[!a$Area %in% part$Area,], part)
   order.a<-order(a$Area)
   a<-a[order.a,]
   return(a)
   } else{

     return(a)
}
  
  
}

popup<-function(x){
  
  z<-switch(x,
            "Parker & grönområden" = c("F1"),
            "Mångfald i bostadsutbudet" =c("F2"),
            "Levandegöra gemensamma platser" =c("F3"),
            "Kommunikationer" = c("F4"),
            "Kultur & fritid" = c("F5"),
            "Utbildning" = c("F6"),
            "Omsorg" = c("F7"),
            "Skolan" = c("F8"),
            "Trygghet" = c("F9"),
            "Hållbar utveckling" = c("F10")
  )
  
  
  z<-subset(fragor,select=z)

  return(z)        
  
}
internal_popup<-function(x,y){
  
  z<-switch(x,
            "Parker & grönområden" = c("F1"),
            "Mångfald i bostadsutbudet" =c("F2"),
            "Levandegöra gemensamma platser" =c("F3"),
            "Kommunikationer" = c("F4"),
            "Kultur & fritid" = c("F5"),
            "Utbildning" = c("F6"),
            "Omsorg" = c("F7"),
            "Skolan" = c("F8"),
            "Trygghet" = c("F9"),
            "Hållbar utveckling" = c("F10")
            
            
  )
  
  z<-subset(fragor,select=z)
  names(y)<-c("Area",t(z))
  return(y)        
  
}

numeric_map<- function(n,m){
 
   z<-switch(n,
             "Välj ett alternativ" = c("Area"), 
             "Vatten eller bostäder" = c("Area","X12"),
             "Service eller grönområden" = c("Area","X13"),
             "Centralort eller mindre tätort" = c("Area","X14")
             )


   a<-subset(m,select=z)
   colnames(a)[2]<-"data"
   antalRader<-nrow(a)
  
   if(!antalRader==43){
     b<- data6
     
     b3<-subset(b,select=z)
     part<-b3[!b3$Area %in% a$Area,]
     colnames(part)[2]<-"data"
     part$data = NA
     a<-rbind(a[!a$Area %in% part$Area,], part)
     order.a<-order(a$Area)
     a<-a[order.a,]
     
     return(a)
   } else{
     

  
     return(a)
   }
   
}

numeric_legend<-function(x){
 
   q<-switch(x,
            "Välj ett alternativ" = c("Area"), 
            "Vatten eller bostäder" = c("F12"),
            "Service eller grönområden" = c("F13"),
            "Centralort eller mindre tätort" = c("F14")
  )
   q1<-subset(fragor2,select=q)
   
   return(q1)
   
  
}
