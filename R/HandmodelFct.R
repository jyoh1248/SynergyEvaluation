#' Sensitivity() Function
#'
#' This function calculates sensitivity of the dose-effect curve. It is the derivated form of effect from dose-effect function.
#' @param x : Effect
#' @param Emin : lower asymptote
#' @param Emax : upper asymptote
#' @param m : slope (Hill's coefficient)
#' @param IC50 : relative IC50
#' @keywords Derivate dose effect curves, sensitivity, Effect-sensitivity
#' @export
#' @examples
#' Sensitivity()
Sensitivity<-function(x,Emin,Emax,m,IC50){
  f<-DoseEffect(x,Emin,Emax,m,IC50)
  val = m/(IC50*(Emax-Emin))*(f-Emin)^(1-1/m)*(Emax-f)^(1+1/m)
  return(val)
}

#' Handmodelwithplot() Function
#'
#' This function calculates sensitivity of the dose-effect curve. It is the derivated form of effect from dose-effect function.
#' @param p : theoretical parameters of doseA and doseB
#' @param lamb : vector of different lambda (not include 0 and 1)
#' @param doseeffect : graphical representation of dose-effect curves
#' @param effectsens : graphical representation of effect-sensitivity curves
#' @param returndata : Return the estimated dose-effect data of the combination. If "F", it returns the parameters.
#' @keywords ]Sensitivity, Handmodel, Synergy
#' @export
#' @examples
#' Sensitivity()
#Output : lambda ,Emin, Emax, slope, IC50 of additive isobole or doseC data
Handmodel<-function(param, lamb, 
                    doseeffect = "F", 
                    effectsens="F",
                    returndata ="F"){
  p = round(param,3)
  #Take parameters
  Emaxa = p$EmaxA
  Emina = p$EminA
  IC50a = exp(p$IC50relA)
  ma = p$mA
  
  Emaxb = p$EmaxB
  Eminb = p$EminB
  IC50b = exp(p$IC50relB)
  mb = p$mB
  
  l = lamb
  nl = length(lamb)
  
  #to store the results
  valEmin = valEmax = valslope = valIC50abs = valIC50rel = rep(0,nl)
  
  min_Emin = min(Emina, Eminb)
  max_Emin = max(Emina, Eminb)
  min_Emax = min(Emaxa, Emaxb)
  max_Emax = max(Emaxa, Emaxb)
  minval = max(min_Emin, 0.001)

  
  #dose-effect curve
  dose = seq(1,3000,1)
  effa<-DoseEffect(dose,Emina,Emaxa,ma,IC50a)
  ya<-data.frame(dose=log(dose), I=effa)
  effb<-DoseEffect(dose,Eminb,Emaxb,mb,IC50b)
  yb<-data.frame(dose=log(dose), I=effb)
  
  plotdose<-ggplot(ya, aes(dose,I))+geom_line()+geom_line(data=yb, aes(dose,I), linetype="dotted")
  plotdoseA<-ggplot(ya, aes(dose,I))+geom_line()+ggtitle("doseA")
  plotdoseB<-ggplot(yb, aes(dose,I))+geom_line()+ggtitle("doseB")
  
  ifelse(doseeffect=="T",print(plotdose),0)

  #effect-sensitivity curve
  sensa<-Sensitivity(effa,Emina,Emaxa,ma,IC50a)
  sa<-data.frame(I=effa, sens=sensa)
  sensb<-Sensitivity(effb,Eminb,Emaxb,mb,IC50b)
  sb<-data.frame(I=effb, sens=sensb)
  
  plotsensA<-ggplot(sa, aes(I,sens))+geom_line()
  plotsensB<-ggplot(sb, aes(I,sens))+geom_line()
  #plotsensAll<-grid.arrange(plotdoseA,plotdoseB,plotsensA,plotsensB,ncol=2)
  
  ifelse(effectsens=="T",grid.arrange(plotdoseA,plotdoseB,plotsensA,plotsensB,ncol=2),0)
  
  for(i in 1:nl){
    lambda = l[i]
    
    #effect-sensitivity curve of combi
    if(min_Emin>0){
      eff1<-seq(0,min_Emin,by = 0.1)
      eff2<-seq(min_Emin,max_Emin, by = 0.1)
    }else if(max_Emin>0){
      eff1<-seq(min_Emin,0,by = 0.1)
      eff2<-seq(0,max_Emin, by = 0.1)
    }else{
      eff1<-NA
      eff2<-NA
    }

    eff3<-seq(max(max_Emin,0),min_Emax,by = 0.1)
    eff4<-seq(min_Emax,max_Emax, by = 0.1)

    sens1 = rep(NA,length(eff1))

    if(min_Emin == max_Emin){
      sens2 = NA
    }else if(min_Emin == Emina){
      sens2 = Sensitivity(eff2,Emina,Emaxa,ma,IC50a)*lambda
    }else if(min_Emin == Eminb){
      sens2 = Sensitivity(eff2,Eminb,Emaxb,mb,IC50b)*(1-lambda)
    }

    sens3 = Sensitivity(eff3,Emina,Emaxa,ma,IC50a)*lambda + Sensitivity(eff3,Eminb,Emaxb,mb,IC50b)*(1-lambda)
    if(Emaxa==Emaxb){
      sens4 = NA
    }else if(min_Emax == Emaxa){
      sens4 = Sensitivity(eff4,Eminb,Emaxb,mb,IC50b)*(1-lambda)
    }else if(min_Emax == Emaxb){
      sens4 = Sensitivity(eff4,Emina,Emaxa,ma,IC50a)*lambda
    }

    
    effect = c(eff1,eff2,eff3,eff4)

    senstotal = c(sens1, sens2, sens3, sens4)

    
    SensC<-data.frame(I=effect, sens=senstotal)
    SensA<-data.frame(I=effa, sens=lambda*sensa)
    SensB<-data.frame(I=effb, sens=(1-lambda)*sensb)

    plotsensA2<-plotsensA+geom_line(data=SensA, aes(I, sens), col="blue")
    plotsensB2<-plotsensB+geom_line(data=SensB, aes(I, sens), col="red")
    
    plotsensC<-ggplot(SensC, aes(I,sens))+geom_line()+
      geom_line(data=SensB, aes(I, sens), col="red")+
      geom_line(data=SensA, aes(I, sens), col="blue")
    
    #Reconstruct the dose-effect curve
    idA<-function(x){IC50a*(Emaxa-Emina)/ma*((Emaxa-x)^(-1-1/ma))*((x-Emina)^(-1+1/ma))}
    idB<-function(x){IC50b*(Emaxb-Eminb)/mb*((Emaxb-x)^(-1-1/mb))*((x-Eminb)^(-1+1/mb))}
    
    idfA<-function(x){1/(lambda/idA(x))}
    idfB<-function(x){1/((1-lambda)/idB(x))}
    idf<-function(x){1/(lambda/idA(x)+(1-lambda)/idB(x))}

    if(min_Emin == max_Emin){
      #print("Shared Emin")
      intpart = idf
      max_Emin = min_Emin = minval
      val1 = 0
      
    }else if((Emina!=Eminb)&&(min_Emin == Emina)){
      #print("Different Emin")
      intpart = idfA
      part1<-ifelse(max_Emin>minval, 
                    integrate(intpart, lower=minval, upper=max_Emin),0)
      val1<-ifelse(class(part1)=="list", part1[[1]],0)
      
    }else if((Emina!=Eminb)&&(min_Emin == Eminb)){
      #print("Different Emin")
      intpart = idfB
      part1<-ifelse(max_Emin>minval, 
                    integrate(intpart, lower=minval, upper=max_Emin),0)
      val1<-ifelse(class(part1)=="list", part1[[1]],0)
    }
    

    maxval = ifelse(floor(min_Emax)==floor(max_Emax),min_Emax-0.1,min_Emax)
    
    part2<-ifelse(max_Emin>minval,
                  integrate(idf,lower=max_Emin, upper=maxval-0.001),NA)
    val2<-ifelse(class(part2)=="list", part2[[1]],0)

    if(Emaxa==Emaxb){
      #print("Shared Emax")
      lastpart = idf
      val3<-0   
    }else if((Emaxa!=Emaxb)&&(min_Emax == Emaxa)){
      #print("Different Emax")
      lastpart = idfB
      part3<-ifelse(max_Emax>min_Emax,
                    integrate(lastpart,lower=min_Emax, upper=max_Emax-0.001),NA)
      val3<-ifelse(class(part3)=="list", part3[[1]],0)
    }else if((Emaxa!=Emaxb)&&(min_Emax == Emaxb)){
      #print("Different Emax, Emaxb>Emaxa")
      lastpart = idfA
      part3<-ifelse(max_Emax>min_Emax,
                    integrate(lastpart,lower=min_Emax, upper=max_Emax-0.001),NA)
      val3<-ifelse(class(part3)=="list", part3[[1]],0)
    }
    #Function that calculate the integration
    
    dosefct_id<-function(x){
      n=length(x)
      dose=rep(0,n)
      
      for(i in 1:n){
        eff<-x[i]

        if(eff>=max_Emin){
          if(eff<min_Emax){
            d<-integrate(idf,lower=max_Emin, upper=eff)
            dose[i]<-d[[1]] + val1
          }else if((Emaxa!=Emaxb)&(eff>=min_Emax)&(eff<max_Emax)){
            d<-integrate(lastpart,lower=max_Emin, upper=eff)
            dose[i]<-d[[1]] + val1 + val2
          }else if(eff>=max_Emax){
            dose[i]<-val1 + val2 + val3
          }
          
        }else if((eff<max_Emin)&(eff>min_Emin)){
          d<-integrate(intpart,lower=minval, upper=eff)
          dose[i]<-d$value
          
        }else if(eff<min_Emin){
          dose[i]<-0
        }
      }
      return(dose)
    }
    Effectvalue<-seq(1,100,0.1)
    Integratedvalue<-dosefct_id(Effectvalue)

    doseC<-data.frame(I=Effectvalue, dose=log(Integratedvalue))
    doseC = doseC %>% filter(dose>0.1)
    #estimate the parameters using the integrated values
    reconC<-drm(I~exp(dose), data=doseC, 
                fct=llogistic2(fixed = c(NA, NA, NA, NA, 1)),type="continuous")
    coefC<-reconC$coefficients
    IC50C<-ED.drc(reconC,50,type = c("absolute"), interval = "fls", display = F)
    
    plotdoseC<-ggplot(doseC, aes(dose,I))+geom_line()+ggtitle("Additivity")
    #plotall <-grid.arrange(plotdoseA,plotdoseB,plotdoseC,
    #                       plotsensA2,plotsensB2,plotsensC,ncol=3,
    #                       top = textGrob(paste("lambda = ", lambda),gp=gpar(fontsize=20,font=3)))
    
    ifelse(effectsens=="T",grid.arrange(plotdoseA,plotdoseB,plotdoseC,
                                        plotsensA2,plotsensB2,plotsensC,ncol=3,
                                        top = textGrob(paste("lambda = ", lambda),gp=gpar(fontsize=20,font=3))),0)
    
    #Store results
    valEmin[i]<-coefC[[2]]
    valEmax[i]<-coefC[[3]]
    valslope[i]<--coefC[[1]]
    valIC50rel[i]<-coefC[[4]]
    valIC50abs[i]<-log(IC50C[[1]])
  }
  result = data.frame(lambda = lamb,
                      Emin = valEmin,
                      Emax = valEmax,
                      m = valslope,
                      IC50abs = valIC50abs,
                      IC50rel = valIC50rel)
  if(returndata=="T"){
    return(doseC)
  }else(
    return(result)
  )
}

#' HandData() Function
#'
#' This function produces the data from parameters obtained from Hand model
#' @param hand : parameters obtained by Hand model
#' @param paramA : parameters of dose A
#' @param paramA : parameters of dose B
#' @param ICvalue : effect (often 50)
#' @keywords Handmodel, Synergy
#' @export
#' @examples
#' HandData()

HandData<-function(hand, p, ICvalue){
  #Take parameters
  EmaxA = p$EmaxA
  EminA = p$EminA
  IC50A= exp(p$IC50relA)
  mA = p$mA
  
  EmaxB = p$EmaxB
  EminB = p$EminB
  IC50B = exp(p$IC50relB)
  mB = p$mB
  
  l = hand$lambda
  
  dA = data.frame(Emin=EminA, Emax=EmaxA, m=mA, IC50rel=log(IC50A))
  IC50absA = InverseDoseEffect(50, EminA, EmaxA, mA, IC50A)
  dA$IC50abs = log(IC50absA)
  doseAalone = InverseDoseEffect(ICvalue, EminA, EmaxA, mA, IC50A)
  dA$dose = doseAalone
  dA$doseA = dA$dose
  dA$doseB = 0
  dA$lambda = 1
  
  dB = data.frame(Emin=EminB, Emax=EmaxB, m=mB, IC50rel=log(IC50B))
  IC50absB = InverseDoseEffect(50, EminB, EmaxB, mB, IC50B)
  dB$IC50abs = log(IC50absB)
  doseBalone = InverseDoseEffect(ICvalue, EminB, EmaxB, mB, IC50B)
  dB$dose = doseBalone
  dB$doseB = dB$dose
  dB$doseA = 0
  dB$lambda = 0
  
  dC = hand
  dC$IC50rel = dC$IC50rel
  dC$IC50abs = dC$IC50abs
  doseC = InverseDoseEffect(ICvalue, hand$Emin, hand$Emax, hand$m, exp(hand$IC50rel))
  doseC_A = doseC*l
  doseC_B = doseC*(1-l)
  dC$dose = doseC
  dC$doseA = doseC_A
  dC$doseB = doseC_B
  
  final = rbind(dC,dA, dB)
  final$I = ICvalue
  return(final)
}


