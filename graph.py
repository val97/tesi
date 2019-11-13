#U=(u1,u2,u3,u4)
#C={(u1,u2,u3), (!u1,!u2,u3), (u2,!u3,u4)}
import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

def problogModel(probModel,node,levels):
    predec=":-"
    observe=""

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
    return probModel,observe

def anglicanModel(anglModel,node,levels):
    prior = "\t"
    probcond = ""
    probcond2 = ""
    observe=""
    clause =   labels[node] +" (cond(or \n\t\t"
    clause2 = ""
    clause3 = ""

    formula = labels[node] +" (cond(and \n\t\t"
    flag=False
    i=0
    for pred in G.predecessors(node):           #i predecessori di un nodo corrispondono ai nodi che lo influenzano.
        isNegate = nx.get_edge_attributes(G,'isNegate')
        i+=1
        if(levels[pred]==-1):
            prob=0.5
            prior = prior + labels[node]+" (sample(flip %s))"%str(prob) +"\n"
            #print("source, ",predec)
        elif(levels[pred]==0):
            if( not flag):
                clause2="(and"
            probcond= "\t)\n\t(sample(flip 1.0)) \n"
            probcond2="\t)\n\t(sample(flip 0.0))) \n"
            if(isNegate[(pred,node)]):
                prior = prior+ clause + "(= %s false)"%labels[pred]+"\n\t"
                clause2= clause2 +  "(= %s true)"%labels[pred]+"\n\t"
            else:
                prior = prior+ clause + "(= %s true)"%labels[pred]+"\n\t"
                clause2= clause2 + "(= %s false)"%labels[pred]+"\n\t"
            clause = "\t"
            flag=True

        else:
            if( not flag):
                clause2="(or"
            probcond= "\t)\n\t(flip 1.0) \n"
            probcond2="\t)\n\t(flip 0.0)) \n"

            prior = prior+ formula + "(= %s true)"%labels[pred]+"\n\t"
            clause2= clause2 +  "(= %s false)"%labels[pred]+"\n\t"
            formula = "\t"
            flag=True

    if(i==1):#se alla fine del ciclo ho una sola condizione tolgo l'and
        print("CIAO CI SONO",node)
        prior = prior.replace("(cond(and \n\t","(cond \n\t")
        probcond = probcond.replace("\t)\n\t(flip 1.0) \n", "\n\t(flip 1.0) \n")
        clause2= clause2.replace("(or","")
        probcond2=probcond2.replace("\t)\n\t(flip 0.0)) \n","\t\n\t(flip 0.0)) \n")
    #print("txt",predec)
    anglModel=anglModel +prior +probcond+clause2+probcond2
    #print("angl",anglModel)
    return anglModel, observe

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
    obsProb=""
    obsAngl=""
    anglModel="""
(ns sat-net \n
    (:require [gorilla-plot.core :as plot] \n
        [anglican.stat :as s])\n
    (:use clojure.repl\n
        [anglican core runtime emit \n
        [inference :only [collect-by]]]))\n
(defquery sat-bayes-net []\n
     (let [\n """
    #print(anglModel)
    while queue:
        node = queue.pop(0)
        if node not in explored:
            levels = nx.get_node_attributes(G,'level')  # positions for all nodes
            if(not (node=="source") ):
                if(not labels[node]=="y"):
                    probModel=problogModel(probModel,node,levels)[0]
                    obsProb = obsProb +problogModel(probModel,node,levels,)[1]
                    anglModel=anglicanModel(anglModel,node,levels)[0]
                    obsAngl=obsAngl + anglicanModel(anglModel,node,levels)[1]
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

    obsProb="(observe y true)"
    anglModel=anglicanModel(anglModel,y,levels)[0]+"]"+obsAngl +"c0))"
    obsProb="evidence(y,true)."
    Model=problogModel(probModel,y,levels)[0]+obsProb
    return Model,anglModel


collection=["(u1,u2,u3)" ,"(not u1,not u2,u3)","(u2,not u3,u4)"]
'''1.0::c0:-u1;u2;u3.
1.0::c1:-\+u1;\+u2;u3.
1.0::c3:-u1;u2;\+u3.
1.0::c4:-u1;\+u2;u3.
1.0::c2:-u2;\+u3;u4.'''
print(collection)
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
anglQuery = """(->> (doquery :lmh sat-bayes-net [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))"""
probQuery = "query(c0)."
probModel = bfs_visit(G,"source")[0]+probQuery
anglModel = bfs_visit(G,"source")[1] +anglQuery

#bisogna aggiungere le evidenze e le query
print(probModel)
print(anglModel)

probFile=open("probSat","w")
probFile.write(probModel)
probFile.close()
anglFile=open("anglSat.clj","w")
anglFile.write(anglModel)
anglFile.close()


plt.axis('off')
plt.show()
