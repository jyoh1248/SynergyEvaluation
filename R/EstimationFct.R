#' EstimateParameters() Function
#'
#' This function estimates the parameters of doseA and doseB from data.
#' @param data : amount of dose 
#' @param nameA : name of dose A
#' @param nameB : name of dose B
#' @keywords Estimate parameters from data, 4P logistic model (free model)
#' @export
#' @examples
#' EstimateParameters()
EstimateParameters<-function(data, nameA, nameB){
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  #Precise the doseB and doseA with their maximal effect
  indB<-as.numeric(data%>%filter(!!as.name(nameA)==0)%>%distinct(Ray))
  indA<-as.numeric(data%>%filter(!!as.name(nameB)==0)%>%distinct(Ray))
  
  #Check the Emin shared --> if the proceed should be done or not
  compA<-data%>%filter(Ray==indA)%>%select(dose =!!as.name(nameA), I = I)
  compB<-data%>%filter(Ray==indB)%>%select(dose =!!as.name(nameB), I = I)
  doseA<-drm(I~dose, data=compA, 
             fct=llogistic2(fixed = c(NA, NA, NA, NA, 1)),type="continuous")
  doseB<-drm(I~dose, data=compB, 
             fct=llogistic2(fixed = c(NA, NA, NA, NA, 1)),type="continuous")
  coefA<-round(doseA$coefficients,3)
  coefB<-round(doseB$coefficients,3)
  
  mA<--coefA[[1]]
  mB<--coefB[[1]]

  EminA<-floor(coefA[[2]])
  EminB<-floor(coefB[[2]])
  
  EmaxA<-ifelse(floor(coefA[[3]])>=99,99,coefA[[3]])
  EmaxB<-ifelse(floor(coefB[[3]])>=99,99,coefB[[3]])
  
  IC50relA<-coefA[[4]]
  IC50relB<-coefB[[4]]

  param = data.frame(EminA = EminA, EmaxA = EmaxA, mA = mA, IC50relA = IC50relA,
                     EminB = EminB, EmaxB = EmaxB, mB = mB, IC50relB = IC50relB)
  
  return(param)
}

#' EstimateValue() Function
#'
#' This function estimates combined doses of each effect from data.
#' @param data : amount of dose 
#' @param nameA : name of dose A
#' @param nameB : name of dose B
#' @param Nexperiment : total number of experiments
#' @param Nray : total number of rays
#' @param ICvalue : effect
#' @keywords Estimate combined doses, 4P logistic model
#' @export
#' @examples
#' EstimateValue()
EstimateValue<-function(data, nameA, nameB, Nexperiment, Nray, ICvalue){
  n = Nexperiment
  nr = c(1:Nray)
  final <- vector("list", Nexperiment)
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  for (i in 1:n){
    Raydata = data %>%filter(Experiment==i)
    indB = as.numeric(Raydata%>%filter(!!as.name(nameA)==0)%>%distinct(Ray))
    indA = as.numeric(Raydata%>%filter(!!as.name(nameB)==0)%>%distinct(Ray))
    
    indmix = nr[-c(indB,indA)]
    Nmix = length(indmix)
    rayvalues = vector("list", Nmix)
    
    for (j in 1:Nmix){
      r=as.numeric(indmix[j])
      mixData = Raydata%>%filter(Ray==r)%>%select(comb,I)
      doseC = drm(I~comb, data= mixData, fct= llogistic2(fixed=c(NA,NA,NA,NA,1)),type="continuous")
      coef=round(doseC$coefficients,3)

      m=-coef[[1]]
      Emin=coef[[2]]
      Emax=coef[[3]]
      IC50=exp(coef[[4]])
      
      value = ED.drc(doseC, ICvalue, interval = "delta", type="absolute",display = FALSE)

      rayvalues[[j]] = data.frame(Ray = r, 
                                  comb= exp(value[[1]]),
                                  ICvalue=ICvalue,
                                  Lower = exp(value[[3]]),
                                  Upper = exp(value[[4]]),
                                  Std.Error = exp(value[[2]]))
    }
    rayval = do.call(rbind,rayvalues)
    final[[i]] = data.frame(rayval, Experiment = i)
  }
  final = do.call(rbind,final)
  return(final)
}

#' CalculateDose() Function
#'
#' This function divide the estimated dose of combined data into dose A and dose B by taking lambda.
#' @param data : estimated dose of the effect
#' @param Lambda : fixed mixture ratio (doseA)
#' @keywords Estimate combined doses
#' @export
#' @examples
#' CalculateDose()

CalculateDose<-function(data, Lambda){
  Raylist = data%>%distinct(Ray)
  n = nrow(Raylist)
  final <- vector("list", n)
  for(i in 1:n){
    r = Raylist$Ray[i]
    l = Lambda[i]
    
    d = data %>% filter(Ray==r)
    d$doseA = d$comb*l
    d$doseB = d$comb*(1-l)
    d$lambda = l
    d$Std.ErrorA = l*d$Std.Error
    d$Std.ErrorB = (1-l)*d$Std.Error
    d$LowerA = l*d$Lower
    d$LowerB = (1-l)*d$Lower
    d$UpperA = l*d$Upper
    d$UpperB = (1-l)*d$Upper
    
    final[[i]]<-d
  }
  final = do.call(rbind,final)
  final$Ray = as.factor(final$Ray)
  return(final)
}

#' EstimatePE() Function
#' @param data : estimated dose of the effect
#' @keywords Estimate combined doses
#' @export
#' @examples
#' EstimatePE()
#' 
EstimatePE<-function(data, nameA, nameB, Nexperiment, Nray){
  n = Nexperiment
  nr = c(1:Nray)
  final <- vector("list", Nexperiment)
  
  Compound_A = as.name(nameA)
  Compound_B = as.name(nameB)
  
  for (i in 1:n){
    Raydata = data %>%filter(Experiment==i)
    indB = as.numeric(Raydata%>%filter(!!as.name(nameA)==0)%>%distinct(Ray))
    indA = as.numeric(Raydata%>%filter(!!as.name(nameB)==0)%>%distinct(Ray))
    
    rayvalues = vector("list", Nray)
    
    for (j in 1:Nray){
      r=as.numeric(nr[j])
      mixData = Raydata%>%filter(Ray==r)%>%select(comb,I)
      doseC = drm(I~comb, data= mixData,
                  fct=llogistic2(fixed=c(NA,NA,NA,NA,1)),type="continuous")
      coef=round(doseC$coefficients,3)
      
      m=-coef[[1]]
      Emin=coef[[2]]
      Emax=coef[[3]]
      IC50=exp(coef[[4]])
      
      rayvalues[[j]] = data.frame(Ray = r, 
                                  Emin = Emin,
                                  Emax = Emax,
                                  m =m,
                                  IC50rel = IC50)
    }
    rayval = do.call(rbind,rayvalues)
    final[[i]] = data.frame(rayval, Experiment = i)
  }
  final = do.call(rbind,final)
  return(final)
}

#' ExtractParameters() Function
#' @keywords Combine parameters of single compoundsinto one dataset
#' @param paramA : parameters of compound A (Emin, Emax, m, IC50rel(exponential value))
#' @param paramb :parameters of compound B (Emin, Emax, m, IC50rel(exponential value))
#' @export
#' @examples
#' ExtractParameters()
#' 
ExtractParameters<-function(paramA, paramB){
  paramTh = data.frame(EminA = paramA$Emin,
                       EmaxA = paramA$Emax,
                       mA = paramA$m,
                       IC50relA = log(paramA$IC50rel),
                       EminB = paramB$Emin,
                       EmaxB = paramB$Emax,
                       mB = paramB$m,
                       IC50relB = log(paramB$IC50rel),
                       ExpA = paramA$Experiment,
                       ExpB = paramB$Experiment)
  return(paramTh)
}
