#' SimulateData() Function
#'
#' This function simulate data with dose = c(0.3, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000). Used inside SingleData(), CombinationData().
#' @param param : Vectors containing parameters(Emin, Emax, m, IC50)
#' @param replicate : number of replicates
#' @param sd : sd
#' @param doseName : name of the component
#' @keywords Simulation
#' @return dose, I, replicate, doseName
#' @export
#' @examples
#' SimulateData()
#' 
SimulateData<-function(param, replicate, sd, doseName){
  doseX = c(0.3,1,3,10,30,100,300,1000,3000,10000)
  final <- vector("list", replicate) 
  
  Emin = param$Emin
  Emax = param$Emax
  m = param$m
  IC50 = param$IC50rel
  
  for (i in 1:replicate){
    Ia = DoseEffect(doseX,Emin,Emax,m,IC50) 
    epsilon = rnorm(length(doseX), 0, sd)
    IA = Ia + epsilon
    
    final[[i]] = data.frame(dose=doseX,
                            I= IA,
                            Replicate = i,
                            N = doseName)
  }
  final = do.call(rbind,final)
  return(final)
}

#' CombinationData() Function
#'
#' This function does simulation with combination data. 
#' @param paramC : Vectors containing parameters(Emin, Emax, m, IC50)
#' @param replicate : number of replicates
#' @param sd : sd
#' @param lambda : fixed mixture ratio of dose A (Ray)
#' @param ray : following ray
#' @keywords Simulation
#' @return I, Replicate, Comb, doseA, doseB, Ray, lambda
#' @export
#' @examples
#' CombinationData()
#' 
CombinationData<-function(paramC, Rep, sd, lambda, ray){
  dataC = SimulateData(paramC, Rep, sd, "C")
  doseC = dataC%>% select(I, Replicate)
  doseC$doseA = lambda*dataC$dose
  doseC$doseB = (1-lambda)*dataC$dose
  doseC$comb = dataC$dose
  doseC$Ray = ray
  doseC$lambda = lambda
  return(doseC)
}

#' SingleData() Function
#'
#' This function does simulation with monotherapy data. 
#' @param param : Vectors containing parameters(Emin, Emax, m, IC50)
#' @param replicate : number of replicates
#' @param sd : sd
#' @param ray : following ray
#' @param doseName : name of the component
#' @keywords Simulation
#' @export
#' @examples
#' SingleData()
SingleData<-function(param, Rep, sd, ray, doseName){
  if(doseName=="A"){
    dataI = SimulateData(param, Rep, sd, "A")
    data = dataI %>% select(I, Replicate)
    data$doseA = dataI$dose
    data$doseB = 0
    data$comb = dataI$dose
    data$Ray = ray
    data$lambda = 1
  }else if(doseName=="B"){
    dataI = SimulateData(param, Rep, sd, "B")
    data = dataI %>% select(I, Replicate)
    data$doseB = dataI$dose
    data$doseA = 0
    data$comb = dataI$dose
    data$Ray = ray
    data$lambda = 0
  }
  return(data)
}

#' SimulateExperiment() Function
#'
#' This function does simulation with multiple experiments.(2 monotherapy data + 3 combination data)
#' @param Nexperiment : total number of experiments
#' @param paramA : Vectors containing parameters(Emin, Emax, m, IC50) of dose A
#' @param paramB : Vectors containing parameters(Emin, Emax, m, IC50) of dose B
#' @param paramC1 : Vectors containing parameters(Emin, Emax, m, IC50) of combined dose
#' @param paramC2 : Vectors containing parameters(Emin, Emax, m, IC50) of combined dose
#' @param paramC3 : Vectors containing parameters(Emin, Emax, m, IC50) of combined dose
#' @param rayA : ray of dose A
#' @param rayB : ray of dose B
#' @param rayC1 : ray of combined dose
#' @param rayC2 : ray of combined dose
#' @param rayC3 : ray of combined dose
#' @param Rep : number of replicates
#' @param sd : sd (same variance for all rays)
#' @keywords Simulation
#' @export
#' @examples
#' SimulateExperiment()
SimulateExperiment<-function(Nexperiment, 
                             paramA, paramB, paramC1,paramC2,paramC3, 
                             lambda1, lambda2, lambda3, rayA, rayB, ray1, ray2, ray3,
                             Rep, sd){
  final <- vector("list", Nexperiment)
  
  for (i in 1:Nexperiment){
    dA = SingleData(paramA,Rep, sd, rayA, "A")
    dB = SingleData(paramB,Rep, sd, rayB, "B")
    dC1 = CombinationData(paramC1,Rep, sd, lambda1, ray1)
    dC2 = CombinationData(paramC2,Rep, sd, lambda2, ray2)
    dC3 = CombinationData(paramC3,Rep, sd, lambda3, ray3)
    
    final[[i]] = data.frame(rbind(dA, dB, dC1, dC2, dC3),
                            Experiment = i)
  }
  final = do.call(rbind,final)
  return(final)
}


