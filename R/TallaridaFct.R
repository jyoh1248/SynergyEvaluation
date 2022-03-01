#' CalculRho() Function
#'
#' This function calculates the necessary rho for the equation of the additive isobole. 
#' @param EmindoseA : lower aymptote of less potency dose
#' @param EmaxdoseA : upper asymptote of less potency dose
#' @param EmindoseB : lower aymptote of higher potency dose
#' @param EmaxdoseB :upper asymptote of higher potency dose
#' @keywords Tallaridamodel, CalculateRho, Additive isobole
#' @export
#' @examples
#' CalculRho()
#Calculate the additive isobole
CalculRho<-function(EmindoseA, EmaxdoseA,EmindoseB, EmaxdoseB){
  EmaxArho<-ifelse(floor(EmaxdoseA)>=99,100,EmaxdoseA)  
  EmaxBrho<-ifelse(floor(EmaxdoseB)>=99,100,EmaxdoseB)

  rho <- (EmaxArho-EmindoseA)/(EmaxBrho-EmindoseB) 
  return(rho)
}

#' AdditiveIsobole() Function
#'
#' This function calculates the additive isobole from Tallarida null model. 
#' @param effect : Effects that additive isobole produce (Often 50)
#' @param sameB : higher potency dose is equal to dose B
#' @param dataB : Dose Effect data of higher potency dose
#' @param IC50A : Relative IC50 of dose A(less potency dose)
#' @param IC50B : Relative IC50 of dose B(higher potency dose)
#' @param mA : slope of dose A(less potency dose)
#' @param mB : slope of dose B(higher potency dose)
#' @param rho : rho obtained from CalculateRho()
#' @keywords Tallaridamodel, Additive isobole
#' @export
#' @examples
#' AdditiveIsobole()
AdditiveIsobole <-function(effect, sameB="T", dataB, IC50A, IC50B, mA, mB, rho){
  #dose B that produce x effect
  if(sameB=="T"){
    dose_B<-dataB %>% filter(effect_B==effect) %>% select(valueB)
  }else if(sameB=="F"){
    dose_B<-dataB %>% filter(effect_A==effect) %>% select(valueA)
  }
  #calculation of dose A
  EminB = 0
  EminA = 0
  
  btotal<-round(dose_B[1,],3)
  b1<-seq(from = 0, to = btotal, by=0.01)
  b2 = btotal-b1
  b3 = 1/(1+(IC50B/b2)^mB)
  a1 = rho/(b3 + EminB - EminA)
  a = IC50A/((a1-1)^(1/mA))
  
  #data_additive
  data<- data.frame(b1,a)
  names(data)<-c("doseB", "doseA")
  data$I<-as.factor(effect)
  return(data)
}

#' Tallaridamodel_Oneline() Function
#'
#' This function calculates additivie isoboles by using Tallarida model (base-line dose is fixed to doseB, higher potency dose)
#' @param param : Theoretical parameters of dose A and doseB
#' @param nameA : Name of dose A(lower potency dose)
#' @param nameB : Name of dose B(higher potency dose)
#' @param eff : Effects (often 50)
#' @keywords Tallaridamodel, Additive isobole
#' @export
#' @examples
#' Tallaridamodel_Oneline()

Tallaridamodel_Oneline<-function(param, nameA, nameB, eff){
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  #Take parameters
  EminA = param$EminA
  EmaxA = param$EmaxA
  mA = param$mA
  ICrel50A = exp(param$IC50relA)
  
  EminB = param$EminB
  EmaxB = param$EmaxB
  mB = param$mB
  ICrel50B = exp(param$IC50relB)
  
  #Dose-effect estimation
  effect_A<-c(floor(EminA):floor(EmaxA))
  valueA<-InverseDoseEffect(effect_A, EminA, EmaxA, mA, ICrel50A)
  A_effect<-data.frame(effect_A,valueA)
  
  effect_B<-c(floor(EminB):floor(EmaxB))
  valueB<-InverseDoseEffect(effect_B, EminB, EmaxB, mB, ICrel50B)
  B_effect<-data.frame(effect_B,valueB)
  
  #Calculate additivity in 2 ways
  namedoseB = Compound_B
  namedoseA = Compound_A
  rho1 = CalculRho(EmindoseA=EminA, EmaxdoseA=EmaxA,EmindoseB=EminB, EmaxdoseB=EmaxB)
  isobole = AdditiveIsobole(effect=eff, sameB="T", dataB=B_effect, IC50A=ICrel50A, 
                             IC50B=ICrel50B, mA=mA, mB=mB, rho=rho1)
  isobole$convert = "B->A"
  
  return(isobole)
}

#' Tallaridamodel_Twoline() Function
#'
#' This function calculates additivie isoboles by using Tallarida model that produce 2 lines of additivity. It should not be used as the 2 lines of additivity will have intersection points. 
#' It is impossible for the pair of doses to produce 2 different additive effects. Only used to graphically represent the two additivities. 
#' @param param : Theoretical parameters of dose A and doseB
#' @param nameA : Name of dose A(lower potency dose)
#' @param nameB : Name of dose B(higher potency dose)
#' @param eff : Effects (often 50)
#' @keywords Tallaridamodel, Additive isobole
#' @export
#' @examples
#' Tallaridamodel_Twoline()
#' 
Tallaridamodel_Twoline<-function(param, nameA, nameB, eff){
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  #Take parameters
  EminA = param$EminA
  EmaxA = param$EmaxA
  mA = param$mA
  ICrel50A = exp(param$IC50relA)
  
  EminB = param$EminB
  EmaxB = param$EmaxB
  mB = param$mB
  ICrel50B = exp(param$IC50relB)
  
  #Dose-effect estimation
  effect_A<-c(floor(EminA):floor(EmaxA))
  valueA<-InverseDoseEffect(effect_A, EminA, EmaxA, mA, ICrel50A)
  A_effect<-data.frame(effect_A,valueA)

  effect_B<-c(floor(EminB):floor(EmaxB))
  valueB<-InverseDoseEffect(effect_B, EminB, EmaxB, mB, ICrel50B)
  B_effect<-data.frame(effect_B,valueB)

  #Calculate additivity in 2 ways
  namedoseB = Compound_B
  namedoseA = Compound_A
  rho1 = CalculRho(EmindoseA=EminA, EmaxdoseA=EmaxA,EmindoseB=EminB, EmaxdoseB=EmaxB)
  isobole1 = AdditiveIsobole(effect=eff, sameB="T", dataB=B_effect, IC50A=ICrel50A, 
                              IC50B=ICrel50B, mA=mA, mB=mB, rho=rho1)
  isobole1$convert = "B->A"
  
  rho2 = CalculRho(EmindoseA=EminB, EmaxdoseA=EmaxB,EmindoseB=EminA, EmaxdoseB=EmaxA)
  isobole2 = AdditiveIsobole(effect=eff, sameB="F", dataB=A_effect, IC50A=ICrel50B, 
                               IC50B=ICrel50A, mA=mB, mB=mA, rho=rho2)
  names(isobole2)<-c("doseA", "doseB" ,"I")
  isobole2$convert = "A->B"
  
  isobole = rbind(isobole1, isobole2)
  return(isobole)
}

#' plotTallarida() Function
#' @keywords Plot tallarida model
#' @export
#' @examples
#' plotTallarida()
plotTallarida<-function(isobole){
  plotisobole<- ggplot(isobole, aes(doseA, doseB, group=convert, linetype=convert)) + 
    geom_line() +
    ggtitle("Additive isobole obtained from Tallarida model")
  print(plotisobole)
}

#' EstimateParametersTallarida() Function
#' @keywords Estimate combined doses
#' @export
#' @examples
#' EstimateParametersTallarida()
EstimateParametersTallarida<-function(data, nameA, nameB, Nexperiment,Nray){
  
  n = Nexperiment
  nr = c(1,Nray)
  final <- vector("list", Nexperiment)
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  for (i in 1:n){
    Raydata = data %>%filter(Experiment==i)
    indB = as.numeric(Raydata%>%filter(!!as.name(nameA)==0)%>%distinct(Ray))
    indA = as.numeric(Raydata%>%filter(!!as.name(nameB)==0)%>%distinct(Ray))
    
    #DoseA
    DataA = Raydata%>%filter(Ray==indA)%>%select(comb,I)
    doseA = drm(I~comb, data= DataA,
                fct=llogistic2(fixed=c(NA,0,NA,NA,1)),type="continuous")
    coefA=round(doseA$coefficients,3)
    
    mA=-coefA[[1]]
    EmaxA=coefA[[2]]
    IC50A=coefA[[3]]
    
    #DoseB
    DataB = Raydata%>%filter(Ray==indB)%>%select(comb,I)
    doseB = drm(I~comb, data= DataB,
                fct=llogistic2(fixed=c(NA,0,NA,NA,1)),type="continuous")
    coefB=round(doseB$coefficients,3)
    
    mB=-coefB[[1]]
    EmaxB=coefB[[2]]
    IC50B=coefB[[3]]
    final[[i]] = data.frame(EminA = 0,
                            mA = mA,
                            EmaxA = EmaxA,
                            IC50relA = IC50A,
                            EminB = 0,
                            mB = mB,
                            EmaxB = EmaxB,
                            IC50relB = IC50B, Experiment = i)
  }
  final = do.call(rbind,final)
  return(final)
}
