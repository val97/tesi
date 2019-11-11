#U=(u1,u2,u3,u4)
#C={(u1,u2,u3), (!u1,!u2,u3), (u2,!u3,u4)}
import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

#faccio una visita del grafo in ampiezza per la scrittura del modello in problog.
def bfs_visit(G ,source):
    explored = []
    # keep track of nodes to be checked
    queue = [source]
    depth=-1
    model=""
    while queue:
        node = queue.pop(0)
        if node not in explored:
            level = nx.get_node_attributes(G,'level')  # positions for all nodes
            if(not (node=="source") ):
                predec=":-"
                for pred in G.predecessors(node):           #i predecessori di un nodo corrispondono ai nodi che lo influenzano.
                    isNegate = nx.get_edge_attributes(G,'isNegate')
                    if(level[pred]==-1):
                        prob=0.5
                        predec=""
                        #model= model + "0.5::"+str(labels[node])+".\n"
                    elif(level[pred]==0):
                        prob=1.0
                        if(isNegate[(pred,node)]):
                            predec=predec+"\+" +labels[pred]+";"
                        else:
                            predec=predec+labels[pred]+";"
                    else:
                        prob=1.0
                        predec=predec+labels[pred]+","
                model=model +str(prob)+ "::"+str(labels[node]) +predec+".\n"

            # add node to list of checked nodes
            explored.append(node)
            neighbours = G.adj[node]
            # add neighbours of node to queue
            for neighbour in neighbours:
                queue.append(neighbour)
    return model.replace(";.",".").replace(",.",".")


collection=["(u1,u2,u3)" ,"(not u1,not u2,u3)","(u2,not u3,u4)"]
print(collection)
labels={}
lastPos=0;
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
     if not k[var] in labels:
       #print(k[var],i+var)
       G.add_node(k[var],pos=(0,lastPos),level=0)
       G.add_edge("source",k[var])
       labels[k[var]]=k[var]
       lastPos+=1
     G.add_edge(k[var],i,isNegate=isNegate)


pos = nx.get_node_attributes(G,'pos')  # positions for all nodes
nx.draw_networkx_nodes(G,pos)
nx.draw_networkx_edges(G,pos)
nx.draw_networkx_labels(G,pos , labels=labels)
program = bfs_visit(G,"source")
#bisogna aggiungere le evidenze e le query
print(program)

plt.axis('off')
plt.show()
