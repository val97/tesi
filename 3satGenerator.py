import os
import numpy as np

#creo un numero random di variabili che sia abbastanza grande

def generate3satFormula():
   numOfVariables=np.random.random_integers(20, 40, size=None)
   print(numOfVariables)
   var=[]
   for i in range(0,numOfVariables):
      var.append('u%i'%i)
   #print(var)

   numOfClauses = int(numOfVariables*4.3)
   #print(numOfClauses)
   formula=[]
   clause="("
   literalExist=False
   boolean=[True,False]
   for i in range(0,numOfClauses):
      #scelgo i tre letterali della clausola
      literal = np.random.choice(var,3,replace=False)
      isNegate=[]
      #random decido se siano negati o meno
      for i in range(0,3):
        isNegate =  np.random.choice(boolean)
        if(isNegate == True):
           clause = clause + "not "+literal[i]+ ","
        else:
           clause = clause + literal[i] + ","
   clause=clause+")"
   formula.append(clause.replace(",)",")"))
   clause="("
   #print(len(formula),formula)


generate3satFormula()
