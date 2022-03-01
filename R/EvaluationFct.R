#' EvaluateAdditivity() Function
#'
#' This function evaluate the combination synergy of the estimated point
#' @param Estfinal : Estimated data 
#' @param Additivity : Additivity data obtained with null model
#' @param nameModel : name of the null model
#' @keywords Evaluate Additivity
#' @export
#' @examples
#' EvaluateAdditivity()
#' 

EvaluateAdditivity<-function(Estfinal, Additivity, nameModel){
  n = nrow(Estfinal)
  final <- vector("list", n)
  for(i in 1:n){
    UA = Estfinal$UpperA[i]
    UB = Estfinal$UpperB[i]
    LA = Estfinal$LowerA[i]
    LB = Estfinal$LowerB[i]
    
    EvalAdd = Additivity %>% filter((doseA<=UA)&(doseA>LA)&(doseB<=UB)&(doseB>LB))
    N1 = nrow(EvalAdd)
    
    if(N1==0){
      Evalsyn = Additivity %>% filter((doseA<UA)&(doseB<UB))
      N2 = nrow(Evalsyn)
      if(N2==0){
        Nsynergy = 1
        Nadditive = 0
        Nantagonism = 0
        Nuncertain = 0
      }else{
        Nsynergy = 0
        Nadditive = 0
        Nantagonism = 1
        Nuncertain = 0
      }
    }else{
      Nsynergy = 0
      Nadditive = 1
      Nantagonism = 0
      Nuncertain = 0
    }
    countsynergy = data.frame(Synergy = Nsynergy, 
                              Additive = Nadditive, 
                              Antagonism = Nantagonism,
                              Uncertain = Nuncertain,
                              model = nameModel)
    final[[i]] = cbind(countsynergy,Estfinal[i,])
  }
  final = do.call(rbind,final)
  return(final)
}

#' EvaluateAdditivity_Tallarida() Function
#'
#' This function evaluate the combination synergy of the estimated point using Tallarida model (two lines)
#' @param Estfinal : estimated data 
#' @param BtoA : Additivity data obtained with Tallarida B to A
#' @param AtoB : Additivity data obtained with Tallarida A to B
#' @param nameModel : name of the null model
#' @keywords Evaluate Additivity
#' @export
#' @examples
#' EvaluateAdditivity_Tallarida()
#' 
EvaluateAdditivity_Tallarida<-function(Estfinal,BtoA, AtoB, nameModel){
  n = nrow(Estfinal)
  final <- vector("list", n)
  for(i in 1:n){
    UA = Estfinal$UpperA[i]
    UB = Estfinal$UpperB[i]
    LA = Estfinal$LowerA[i]
    LB = Estfinal$LowerB[i]
    
    EvalAdd = BtoA %>% filter((doseA<UA)&(doseA>LA)&(doseB<UB)&(doseB>LB))
    N1 = nrow(EvalAdd)
    
    if(N1==0){
      EvalsynB = BtoA %>% filter((doseA<UA)&(doseB<UB))
      EvalsynA = AtoB %>% filter((doseA<UA)&(doseB<UB))
      Nb1 = nrow(EvalsynB)
      Na1 = nrow(EvalsynA)
      
      EvalantA = AtoB %>% filter((doseA>LA)&(doseB>LB))
      EvalantB = BtoA %>% filter((doseA>LA)&(doseB>LB))
      Na2 = nrow(EvalantA)
      Nb2 = nrow(EvalantB)
      
      if((Nb1==0)&&(Na1==0)){
        Nsynergy = 1
        Nadditive = 0
        Nantagonism = 0
        Nuncertain = 0
      }else if((Nb2==0)&&(Na2==0)){
        Nsynergy = 0
        Nadditive = 0
        Nantagonism = 1
        Nuncertain = 0
      }else{
        Nsynergy = 0
        Nadditive = 0
        Nantagonism = 0
        Nuncertain = 1
      }
    }else{
      Nsynergy = 0
      Nadditive = 0
      Nantagonism = 0
      Nuncertain = 1
    }
    countsynergy = data.frame(Synergy = Nsynergy, 
                              Additive = Nadditive, 
                              Antagonism = Nantagonism,
                              Uncertain = Nuncertain,
                              model = nameModel)
    final[[i]] = cbind(countsynergy,Estfinal[i,])
  }
  final = do.call(rbind,final)
  return(final)
}

#' CountAdditivity() Function
#'
#' This function count the combination synergy of the estimated point
#' @param Estfinal : estimated data 
#' @param Additivity : Additivity data obtained with null model
#' @param nameModel : name of the null model
#' @keywords count Additivity
#' @export
#' @examples
#' CountAdditivity()
#' 
CountAdditivity<-function(Estfinal, Additivity, nameModel){
  raylist = levels(Estfinal$Ray)
  Kilist = levels(Estfinal$Ki)
  
  Nray = length(raylist)
  Nki = length(Kilist)
  final <- vector("list", Nki)

  for(j in 1:Nki){
    ki = Kilist[j]
    data1 = Estfinal %>% filter(Ki==ki)
    result <- vector("list", Nray)
    for(i in 1:Nray){
      ray = raylist[i]
      data2 = data1 %>% filter(Ray==ray)
      data = EvaluateAdditivity(data2, Additivity, nameModel)
      
      n = nrow(data)
      Nad = sum(data$Additive)
      Nsy = sum(data$Synergy)
      Nan = sum(data$Antagonism)
      Nun = sum(data$Uncertain)
      
      result[[i]] = data.frame(N = n, 
                               Model = nameModel,
                               Synergy = Nsy,
                               Additive = Nad,
                               Antagonism = Nan,
                               Uncertain = Nun,
                               Ray = ray,
                               Ki = ki) 
      
    }
    result = do.call(rbind,result)
    final[[j]] = result
  }
  final = do.call(rbind,final)
  
  return(final)
}

#' CountAdditivity_Tallarida() Function
#'
#' This function count the combination synergy of the estimated point for Tallarida model (twolines)
#' @param Estfinal : estimated data 
#' @param BtoA : Additivity Tallarida B to A
#' @param AtoB : Additivity Tallarida A to B
#' @param nameModel : name of the null model
#' @keywords count Additivity
#' @export
#' @examples
#' CountAdditivity_Tallarida()
#' 
CountAdditivity_Tallarida<-function(Estfinal, BtoA, AtoB, nameModel){
  raylist = levels(Estfinal$Ray)
  Kilist = levels(Estfinal$Ki)
  
  Nray = length(raylist)
  Nki = length(Kilist)
  final <- vector("list", Nki)
  
  for(j in 1:Nki){
    ki = Kilist[j]
    data1 = Estfinal %>% filter(Ki==ki)
    result <- vector("list", Nray)
    for(i in 1:Nray){
      ray = raylist[i]
      data2 = data1 %>% filter(Ray==ray)
      data = EvaluateAdditivity_Tallarida(data2, BtoA, AtoB, nameModel)
       
      n = nrow(data)
      Nad = sum(data$Additive)
      Nsy = sum(data$Synergy)
      Nan = sum(data$Antagonism)
      Nun = sum(data$Uncertain)
      
      result[[i]] = data.frame(N = n, 
                               Model = nameModel,
                               Synergy = Nsy,
                               Additive = Nad,
                               Antagonism = Nan,
                               Uncertain = Nun,
                               Ray = ray,
                               Ki = ki) 
      
    }
    result = do.call(rbind,result)
    final[[j]] = result
  }
  final = do.call(rbind,final)
  
  return(final)
}


#' FinalAdditivity() Function
#'
#' Combine the result obtained from all null models
#' @param Estfinal : estimated data 
#' @param Loewe : Additivity Loewe
#' @param TallaridaBtoA : Additivity Tallarida B to A
#' @param TallaridaAtoB : Additivity Tallarida A to B
#' @param Hand : Additivity Hand
#' @param HSA : Additivity HSA
#' @keywords count Additivity
#' @export
#' @examples
#' FinalAdditivity()
#' 

FinalAdditivity<-function(Estfinal, Loewe, TallaridaBtoA, TallaridaAtoB, Hand){
  l = CountAdditivity(Estfinal, Loewe, "Loewe")
  t1 = CountAdditivity(Estfinal, TallaridaBtoA, "Tallarida(B->A)")
  t2 = CountAdditivity(Estfinal, TallaridaAtoB, "Tallarida(A->B)")
  h = CountAdditivity(Estfinal, Hand, "Hand")
  ttotal = CountAdditivity_Tallarida(Estfinal, TallaridaBtoA, TallaridaAtoB, "Tallarida")
  
  final = rbind(l, h, ttotal, t1, t2)
  return(final)
}
