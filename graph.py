#U=(u1,u2,u3,u4)
#C={(u1,u2,u3), (!u1,!u2,u3), (u2,!u3,u4)}
import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

def bfs_visit(G ,source):
    explored = []
    # keep track of nodes to be checked
    queue = [source]
    depth=-1
    program=""
    while queue:
        #print("queue",queue)
        node = queue.pop(0)
        if node not in explored:
            level = nx.get_node_attributes(G,'level')  # positions for all nodes
            #print("level node "+str(node)+":"+str(level[node]))
            if(not (node=="source") ):
                predec=":-"
                for pred in G.predecessors(node):
                    isNegate = nx.get_edge_attributes(G,'isNegate')
                    if(level[pred]==-1):
                        prob=0.5
                        predec=""
                        #program= program + "0.5::"+str(labels[node])+".\n"
                    elif(level[pred]==0):
                        prob=1.0
                        if(isNegate[(pred,node)]):
                            predec=predec+"\+" +labels[pred]+";"
                        else:
                            predec=predec+labels[pred]+";"
                    else:
                        prob=1.0
                        predec=predec+labels[pred]+","
                program=program +str(prob)+ "::"+str(labels[node]) +predec+".\n"
                #print(str(G.predecessors(node)))
                #print(next(G.predecessors(node)))
                #program.replace(",.",".")

            # add node to list of checked nodes
            explored.append(node)
            neighbours = G.adj[node]
            #print("neighbours",node,neighbours)
            #print("number ",len(neighbours))

            # add neighbours of node to queue
            for neighbour in neighbours:
                queue.append(neighbour)
    return explored,program.replace(";.",".").replace(",.",".")


collection=["(u1,u2,u3)" ,"(not u1,not u2,u3)","(u2,not u3,u4)","(u1,u2,not u3)","(u1,not u2,u3)"]
print(collection)
labels={}
lastPos=0;
G = nx.DiGraph()
G.add_node("source", pos=(-1,0),level=-1)
labels["source"]="source"

for i in range(0, len(collection)):
  # print(len(collection),i)
   k=collection[i].replace("(","").replace(")","").split(",")    #prendo tutte le variabil di C_i
   print(k)
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
print(bfs_visit(G,"source")[1])

plt.axis('off')
plt.show()
