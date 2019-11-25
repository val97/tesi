#!/bin/bash
for VARIABLES in 10 20 30 40 50
    do
      for TIMES in 0.5 1 1.5 2 2.5 3 3.5 4 4.3 5 5.5
        do
          cd /home/valentina/tesi/
          python3 graph.py ${VARIABLES} ${TIMES}
          MYVAR=$((time problog "probSat${VARIABLES}.pl" -k sdd) |&  tee probResult.out)
          resultP=$(echo $MYVAR | cut -f 1-2 -d " ")
          timeP=$(echo $MYVAR | cut -f 3-8 -d " ")
          ratio=$( bc -l <<< "scale=2; ($VARIABLES*$TIMES)/$VARIABLES")
          echo "$VARIABLES,$ratio,$resultP, $timeP ">>resultsProblog.csv
          cd /home/valentina/Desktop/anglican1/anglsat/
          MYVAR=$((time lein run) |& tee angResult.out)
          resultA=$(echo $MYVAR | cut -f 1-2 -d " ")
          timeA=$(echo $MYVAR | cut -f 3-8 -d " ")
          echo "$VARIABLES, $ratio, $resultA, $timeA ">>/home/valentina/tesi/resultsAnglican.csv
        done
    done
#cat probResult.out | grep real | cut -d " " -f 4 | sed 's/m/:/' | cut -d \. -f1 | sed s/:/*60+/g | bc
#scrive l' orario in secondi
