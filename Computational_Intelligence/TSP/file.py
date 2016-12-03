from numpy  import *
 
berlin52 = loadtxt('berlin52.tsp')  
site={}
count_0=0
for i in berlin52:
    site[str(int(count_0))]=[i[0],i[1]]
    count_0=count_0+1

d={}
count_1=0
for i in berlin52:
    d[str(int(count_1))]=count_1
    count_1=count_1+1

print(d)
print(site)
"""
# mapeando cidades para números
d = {'A':1, 'B':2, 'C':3, 'D':4, 'E':5, 'F':6, 'G':7, 'H':8}

# adiciona as arestas
graph.addEdge(d['B'], d['A'], 42.5)
graph.addEdge(d['A'], d['B'], 42.5)
graph.addEdge(d['C'], d['A'], 61.5)
graph.addEdge(d['A'], d['C'], 61.5)
graph.addEdge(d['C'], d['B'], 14.5)
graph.addEdge(d['B'], d['C'], 14.5)
graph.addEdge(d['D'], d['A'], 30.5)
graph.addEdge(d['A'], d['D'], 30.5)
graph.addEdge(d['D'], d['B'], 87.5)
graph.addEdge(d['B'], d['D'], 87.5)
graph.addEdge(d['D'], d['C'], 20.5)
graph.addEdge(d['C'], d['D'], 20.5)
graph.addEdge(d['E'], d['A'], 17.5)
graph.addEdge(d['A'], d['E'], 17.5)
graph.addEdge(d['E'], d['B'], 28.5)
graph.addEdge(d['B'], d['E'], 28.5)
graph.addEdge(d['E'], d['C'], 81.5)
graph.addEdge(d['C'], d['E'], 81.5)
graph.addEdge(d['E'], d['D'], 34.5)
graph.addEdge(d['D'], d['E'], 34.5)
graph.addEdge(d['F'], d['A'], 82.5)
graph.addEdge(d['A'], d['F'], 82.5)
graph.addEdge(d['F'], d['B'], 70.5)
graph.addEdge(d['B'], d['F'], 70.5)
graph.addEdge(d['F'], d['C'], 21.5)
graph.addEdge(d['C'], d['F'], 21.5)
graph.addEdge(d['F'], d['D'], 33.5)
graph.addEdge(d['D'], d['F'], 33.5)
graph.addEdge(d['F'], d['E'], 41.5)
graph.addEdge(d['E'], d['F'], 41.5)
graph.addEdge(d['G'], d['A'], 31.5)
graph.addEdge(d['A'], d['G'], 31.5)
graph.addEdge(d['G'], d['B'], 19.5)
graph.addEdge(d['B'], d['G'], 19.5)
graph.addEdge(d['G'], d['C'], 8.5)
graph.addEdge(d['C'], d['G'], 8.5)
graph.addEdge(d['G'], d['D'], 91.5)
graph.addEdge(d['D'], d['G'], 91.5)
graph.addEdge(d['G'], d['E'], 34.5)
graph.addEdge(d['E'], d['G'], 34.5)
graph.addEdge(d['G'], d['F'], 19.5)
graph.addEdge(d['F'], d['G'], 19.5)
graph.addEdge(d['H'], d['A'], 11.5)
graph.addEdge(d['A'], d['H'], 11.5)
graph.addEdge(d['H'], d['B'], 33.5)
graph.addEdge(d['B'], d['H'], 33.5)
graph.addEdge(d['H'], d['C'], 29.5)
graph.addEdge(d['C'], d['H'], 29.5)
graph.addEdge(d['H'], d['D'], 10.5)
graph.addEdge(d['D'], d['H'], 10.5)
graph.addEdge(d['H'], d['E'], 82.5)
graph.addEdge(d['E'], d['H'], 82.5)
graph.addEdge(d['H'], d['F'], 32.5)
graph.addEdge(d['F'], d['H'], 32.5)
graph.addEdge(d['H'], d['G'], 59.5)
graph.addEdge(d['G'], d['H'], 59.5)
"""