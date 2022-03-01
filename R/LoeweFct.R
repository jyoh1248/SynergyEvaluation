#' Loewemodel() Function
#'
#' This function calculates additivie isoboles by using classical Loewe model. It prints the combination doses with different mixture ratio. 
#' The result only contains doseA, doseB, I. The calculation of doseA and doseB is not influenced by Ki or lambda.
#' @param param : Theoretical parameters of dose A and doseB
#' @param lambda : fixed mixture ratio to print the combinations.
#' @param Ki : Combination Index = 1 for additivity.
#' @param ICvalue : Effects (often 50)
#' @keywords Tallaridamodel, Additive isobole
#' @export
#' @examples
#' Loewemodel()
#' 
Loewemodel<-function(ICvalue, param, lambda, Ki=1){
  
  #Take parameters
  EminA = param$EminA
  EmaxA = param$EmaxA
  mA = param$mA
  ICrel50A = exp(param$IC50relA)
  
  EminB = param$EminB
  EmaxB = param$EmaxB
  mB = param$mB
  ICrel50B = exp(param$IC50relB)

  ICA<-InverseDoseEffect(ICvalue, EminA, EmaxA, mA, ICrel50A)
  ICB<-InverseDoseEffect(ICvalue, EminB, EmaxB, mB, ICrel50B)

  #Value Combi
  Comb = Ki/(lambda/ICA+(1-lambda)/ICB)
  CA = Comb*lambda
  CB = Comb*(1-lambda)

  doseA = seq(0,ICA,0.01)
  doseB = ICB-(ICB/ICA)*doseA
  
  result = data.frame(ICvalue = ICvalue,
                      ICA = ICA,
                      ICB = ICB,
                      CA= CA,
                      CB= CB,
                      Ki = Ki)
  #print(result)
  dose = data.frame(doseA = doseA,
                    doseB = doseB,
                    I = ICvalue)
  return(dose)
}
