#!/usr/bin/python3
import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import pycosat
import sys



def check3SatSatisfiability(collection):
    trialcnf=[]
    for i in range(0,len(collection)):
        var=collection[i].replace("(","").replace(")","").split(",")
        clause=[]
        for j in range(0,len(var)):
            clause.append(int(var[j].replace("u","").replace("not ","-")))
        trialcnf.append(clause)
    print(trialcnf)

    return pycosat.solve(trialcnf)

def generate3satFormula(varNum, ratio):
   numOfVariables=varNum#np.random.random_integers(10, 20, size=None)
   print(numOfVariables)
   var=[]
   for i in range(0,numOfVariables):
      index=i+1
      var.append('u%i'%index)
   #print(var)

   numOfClauses = int(numOfVariables * ratio )
   #print(numOfClauses)
   formula=[]
   clause="("
   literalExist=False
   boolean=[True,False]
   for i in range(0,numOfClauses):
      #scelgo i tre letterali della clausola
      literal = np.random.choice(var,3,replace=False)
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
   print(len(formula),formula)
   return formula

def problogModel(probModel,node,levels):
    predec=":-"
    #observe=""
    for pred in G.predecessors(node):           #i predecessori di un nodo corrispondono ai nodi che lo influenzano.
        isNegate = nx.get_edge_attributes(G,'isNegate')
        if(levels[pred]==-1):
            prob=0.5
            predec=""
            #model= model + "0.5::"+str(labels[node])+".\n"
        elif(levels[pred]==0):
            prob=1.0
            if(isNegate[(pred,node)]):
                predec=predec+"\+" +labels[pred]+";"
            else:
                predec=predec+labels[pred]+";"
            #observe="evidence( "+labels[node]+", true).\n"
        else:
            prob=1.0
            predec=predec+labels[pred]+","

    probModel=probModel +str(prob)+ "::"+str(labels[node]) +predec+".\n"
    probModel=probModel.replace(";.",".").replace(",.",".")
    #print(observe)
    return probModel

def anglicanModel(anglModel,node,levels):
    observe="" #da togliere
    prior = "\t"
    i=0
    val=[]
    infl=[]
    i=0
    for pred in G.predecessors(node):           #i predecessori di un nodo corrispondono ai nodi che lo influenzano.

        isNegate = nx.get_edge_attributes(G,'isNegate')
        if(levels[pred]==-1):
            prob=0.5
            prior = prior + labels[node]+" (sample(flip %s))"%str(prob) +"\n"
            #print("source, ",predec)
        elif (levels[pred]==0):

            infl.append(labels[pred])
            if(isNegate[(pred,node)]):
                val.append("false")
            else:
                val.append("true")
            #print(infl)
            if(i==2):
                prior=prior + labels[node] + " (getPrior " + infl[0]+ " " + infl[1]+ " " + infl[2] + " " + val[0]+ " " +val[1]+ " " +val[2] +")\n"
            i+=1
        else:
            infl.append(labels[pred])
            prior=prior+""
            i+=1
            if(i==2):
                prior=prior + labels[node] + " (getY " + infl[0]+ " " + infl[1]+ " " + ")\n"
    if(i==1):
        prior=prior + labels[node] + "(cond(= "+labels[pred] +" true)\n(sample(flip 1.0))\n(= "+labels[pred] +" false)\n(sample(flip 0.0)))\n"
    anglModel=anglModel+prior
    #print("angl",anglModel)
    return anglModel

def quicksort(x):
    if len(x) == 1 or len(x) == 0:
        return x
    else:
        pivot = x[0]
        i = 0
        for j in range(len(x)-1):
            if x[j+1] < pivot:
                x[j+1],x[i+1] = x[i+1], x[j+1]
                i += 1
        x[0],x[i] = x[i],x[0]
        first_part = quicksort(x[:i])
        second_part = quicksort(x[i+1:])
        first_part.append(x[i])
        return first_part + second_part

#faccio una visita del grafo in ampiezza per la scrittura del modello in problog.
def bfs_visit(G ,source):
    explored = []
    # keep track of nodes to be checked
    queue = [source]
    depth=-1
    y=""
    probModel=""

    anglModel="""
(ns anglsat.core
(:require
        [anglican.stat :as s])
(:use [anglican.core :refer [doquery]]

       [anglican runtime emit
       [inference :only [collect-by] ]]))
    (defm getPrior[u1 u2 u3 val1 val2 val3]
        (let[
            c (cond(or
		          (= u1 val1)
		          (= u2 val2)
		          (= u3 val3))
	          (sample(flip 1.0))
              (and(not= u1 val1)
                  (not= u2 val2)
                  (not= u3 val3))
    	      (sample(flip 0.0)))]c))
    (defm getY[u1 u2]
        (let[
            c (cond(and
                  (= u1 true)
                  (= u2 true))

              (sample(flip 1.0))
              (or(= u1 false)
                  (= u2 false))
              (sample(flip 0.0)))]c))
(defquery anglsat []\n
     (let [\n """
    #print(anglModel)
    while queue:
        node = queue.pop(0)
        if node not in explored:
            levels = nx.get_node_attributes(G,'level')  # positions for all nodes
            if(not (node=="source") ):
                if(not labels[node]=="y"):
                    #print("esploro",node)
                    probModel=problogModel(probModel,node,levels)
                    #obsProb = obsProb +problogModel(probModel,node,levels,)[1]
                    anglModel=anglicanModel(anglModel,node,levels)
                    #obsAngl=obsAngl + anglicanModel(anglModel,node,levels)[1]
                else:
                    y=node
                    #print(node)
            # nodeadd node to list of checked nodes

            explored.append(node)
            neighbours = G.adj[node]
            # add neighbours of node to queue
            for neighbour in neighbours:

                queue.append(neighbour)
                queue = quicksort(queue)

#    obsProb="(observe y true)"
    anglModel=anglicanModel(anglModel,y,levels)+"]" +"y))"
#    obsProb="evidence(y,true)."
    Model=problogModel(probModel,y,levels)
    return Model,anglModel

#this is an unsat formula
#collection=['(u1,not u2,not u13)', '(not u12,u5,u6)', '(u6,not u1,not u2)', '(not u6,not u14,u8)', '(u18,not u11,u13)', '(u8,u10,u12)', '(u18,not u17,not u6)', '(u8,not u16,u5)', '(not u8,u15,not u10)', '(not u3,u7,u13)', '(not u4,u8,not u10)', '(not u20,not u7,not u2)', '(not u17,u15,u5)', '(u10,not u15,u11)', '(not u5,u8,u15)', '(u9,not u4,u14)', '(not u19,not u16,not u13)', '(u5,u6,not u10)', '(u8,u11,not u16)', '(not u7,not u12,not u13)', '(u17,u12,not u14)', '(u9,u20,not u13)', '(u18,not u17,u15)', '(not u4,not u18,not u6)', '(u2,not u4,u9)', '(u17,u7,u19)', '(not u15,u9,not u11)', '(not u2,u13,not u15)', '(not u13,u2,not u19)', '(not u14,not u9,u17)', '(not u18,not u20,not u2)', '(not u2,not u16,u10)', '(not u3,u5,u4)', '(not u4,not u7,not u10)', '(u8,u6,u17)', '(not u16,u12,not u13)', '(u5,u16,not u19)', '(u4,not u8,not u17)', '(u15,u12,u6)', '(not u2,u14,not u19)', '(not u8,not u7,u18)', '(u17,not u3,u6)', '(u14,not u18,not u13)', '(not u15,u8,u9)', '(u19,not u5,u10)', '(u20,not u6,u14)', '(u1,not u11,not u20)', '(not u5,not u17,u4)', '(u6,not u3,not u4)', '(u4,u13,not u6)', '(not u1,u9,u3)', '(not u20,not u3,not u1)', '(u19,not u12,u17)', '(not u12,not u13,u16)', '(u16,not u10,u6)', '(u4,u14,not u9)', '(not u6,u12,u18)', '(u19,u5,u17)', '(not u6,not u5,u17)', '(u14,not u11,not u7)', '(u20,u6,not u19)', '(not u7,u6,u20)', '(u18,u15,not u3)', '(u18,not u19,u14)', '(not u15,u20,u8)', '(not u2,u12,not u20)', '(not u16,u20,not u4)', '(not u9,u16,u20)', '(not u11,u9,u8)', '(u20,u13,u2)', '(not u20,not u5,not u8)', '(u15,u12,not u13)', '(not u18,u14,u15)', '(u19,u2,not u12)', '(u10,not u9,not u11)', '(u8,not u17,u13)', '(u7,u14,u11)', '(u11,not u20,u8)', '(u9,u17,not u2)', '(u1,u4,u19)', '(u9,u14,not u2)', '(not u18,u7,u12)', '(not u7,u8,u16)', '(not u1,not u20,u15)', '(u14,not u15,u1)', '(u5,not u3,not u18)']
#this is a shorter unsat formula
#collection=['(u1,u2,u3)','(u1,u2,not u3)','(u1,not u2,u3)','(u1,not u2,not u3)','(not u1,u2,u3)''(not u1,u2,not u3)','(not u1,not u2,u3)','(not u1,not u2,u3)','( u4,not u2,not u3)']
varNum= int(sys.argv[1])
ratio= float(sys.argv[2])
collection=generate3satFormula(varNum, ratio)
print(len(collection))
labels={}
lastPos=0
index=0
G = nx.DiGraph()
G.add_node("source", pos=(-1,0),level=-1)       #il nodo "source" è un nodo di appoggio.
labels["source"]="source"

for i in range(0, len(collection)):
  # print(len(collection),i)
   k=collection[i].replace("(","").replace(")","").split(",")    #prendo tutte le variabil di C_i
   #creo un nodo per ogni clausola di C e un dummyNode, il dummyNode mi è un nodo d'appoggio che uso per alleggerire i nodi entranti in Y
   #corrispondono a clause satisfaction testing component e overall satisfaction testing component

   G.add_node(i, pos=(1,i),level=1)
   labels[i]="c%i" %i
   dummyNode=len(collection)+i

   G.add_edge(i,dummyNode)
   G.add_node(dummyNode,pos=(2,i),level=2)
   if(i!=len(collection)-1):
      G.add_edge(dummyNode,dummyNode+1)
      labels[dummyNode]="d%i" %i
   else:
#il nodo Y è influenzato da tutte le variabili del grafo e rispecchierà la soddisfacibilità della formula C
        labels[dummyNode]="y"
   for var in range(0,len(k)):
     isNegate=False
        #trovo le foglie (truth setting component)
     if(k[var]==("not "+k[var].replace("not ",""))):
       isNegate=True
       k[var]=k[var].replace("not ","")
     index=0 - int(k[var].replace("not ","").replace("u",""))
     if not index in labels:

       G.add_node(index,pos=(0,lastPos),level=0)
       G.add_edge("source",index)
       labels[index]=k[var]
       lastPos+=1
     G.add_edge(index,i,isNegate=isNegate)


pos = nx.get_node_attributes(G,'pos')  # positions for all nodes
nx.draw_networkx_nodes(G,pos,with_label=True)
nx.draw_networkx_edges(G,pos)
nx.draw_networkx_labels(G,pos , labels=labels)
anglQuery = """(defn -main [& args]
  (println
   (->> (doquery :lmh anglsat [true true]
                 :number-of-particles 100)
        (take 1000)
        (map #(vector
               (:result %)
               (:log-weight %)))
 (s/empirical-distribution)
        )))"""
probQuery = "query(y)."
probModel = bfs_visit(G,"source")[0]+probQuery
anglModel = bfs_visit(G,"source")[1] +anglQuery

#bisogna aggiungere le evidenze e le query
#print(probModel)
#print("anglmodel",anglModel)

probFile=open("probSat"+str(varNum)+".pl","w")
probFile.write(probModel)
probFile.close()
anglFile=open("/home/valentina/Desktop/anglican1/anglsat/src/anglsat/core.clj","w")
anglFile.write(anglModel)
anglFile.close()
result= check3SatSatisfiability(collection)
print(result)


plt.axis('off')
#plt.show()
