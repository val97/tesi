import os
import sys
import time
import numpy as np
import subprocess as sp

#bashCommand = "cwm --rdf test.rdf --ntriples > test.nt"
#os.system(bashCommand)
def takeTime(command):
     start = time.time()
     results = sp.getoutput(command)
     elapsedTime = (time.time() - start)
     return results,elapsedTime

for variables in range(20, 50, 5):
      for times in np.arange(1, 8, 0.5):
          ratio = (variables* times)/ variables
          averageP = 0
          averageA = 0
          for i in range(0,3):
              os.chdir("/home/valentina/tesi/")
              bashCommand = "python3 graph.py "+str(variables)+" "+str(times)
              os.system(bashCommand)
              results= takeTime("problog probSat" +str(variables) +".pl -k sdd")
              averageP = averageP +results[1]
              with open('resultsProblog.csv', 'a') as f:
                  f.write(str(variables)+", "+str(ratio)+", "+ str(results[1])+", "+str(results[0]) +"\n")
              f.close()

              os.chdir("/home/valentina/Desktop/anglican1/anglsat/")
              results= takeTime("lein run")
              averageA = averageA +results[1]
              with open('/home/valentina/tesi/resultsAnglican.csv', 'a') as f:
                  f.write(str(variables)+", "+str(ratio)+", "+str(results[1])+", "+ str(results[0])+"\n")
              f.close()

          with open('/home/valentina/tesi/timeProblog.csv', 'a') as f:
              f.write(str(variables)+", "+str(ratio)+", "+ str(averageP/3)+"\n")
          f.close()
          with open('/home/valentina/tesi/timeAnglican.csv', 'a') as f:
               f.write(str(variables)+", "+str(ratio)+", "+ str(averageA/3)+"\n")
          f.close()

#scrive l' orario in secondi
