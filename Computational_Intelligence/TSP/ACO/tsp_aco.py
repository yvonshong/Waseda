# encoding:utf-8

'''
	Ant Colony Optimization for Traveling Salesman Problem
'''

import random, math
from numpy  import *
import math

# classe to present an edge
class Edge:

	def __init__(self, origin, destination, cost):
		self.origin = origin
		self.destination = destination
		self.cost = cost
		self.pheromone = None

	def getOrigin(self):
		return self.origin

	def getDestination(self):
		return self.destination

	def getCost(self):
		return self.cost

	def getPheromone(self):
		return self.pheromone

	def setPheromone(self, pheromone):
		self.pheromone = pheromone



class Graph:

	def __init__(self, num_vertices):
		self.num_vertices = num_vertices # The number of vertices of the graph
		self.edges = {} # The dictionary of the edges
		self.neighbors = {} # The dictionary of all the neighbors of each vertex


	def addEdge(self, origin, destination, cost):
		edge = Edge(origin=origin, destination=destination, cost=cost)
		self.edges[(origin, destination)] = edge
		if origin not in self.neighbors:
			self.neighbors[origin] = [destination]
		else:
			self.neighbors[origin].append(destination)
		
		edge_symmetry = Edge(origin=destination, destination=origin, cost=cost) # add the symmetric path with the same cost
		self.edges[(destination,origin)] = edge_symmetry
		if destination not in self.neighbors:
			self.neighbors[destination] = [origin]
		else:
			self.neighbors[destination].append(origin)
		


	def getCostEdge(self, origin, destination):
		return self.edges[(origin, destination)].getCost()

	def getPheromoneEdge(self, origin, destination):
		return self.edges[(origin, destination)].getPheromone()

	def setPheromoneEdge(self, origin, destination, pheromone):
		self.edges[(origin, destination)].setPheromone(pheromone)

	def getCostPath(self, path):
		cost = 0
		for i in range(self.num_vertices - 1):
			cost += self.getCostEdge(path[i], path[i+1])
		# Add the final cost
		cost += self.getCostEdge(path[-1], path[0])
		return cost


class GraphComplete(Graph):
	# Generates a graph complete
	def generate(self):
		for i in range(0, self.num_vertices):
			for j in range(0, self.num_vertices):
				if i != j:
					peso = random.randint(1, 10)
					self.addEdge(i, j, peso)



class Ant:

	def __init__(self, city):
		self.city = city
		self.solution = []
		self.cost = None

	def getCity(self):
		return self.city

	def setCity(self, city):
		self.city = city

	def getSolution(self):
		return self.solution

	def setSolution(self, solution, cost):
		# update the solution
		if not self.cost:
			self.solution = solution[:]
			self.cost = cost
		else:
			if cost < self.cost:
				self.solution = solution[:]
				self.cost = cost

	def getCostSolution(self):
		return self.cost


class ACO:

	def __init__(self, graph, num_ants, alpha=1.0, beta=5.0, iterations=10, evaporationRate=0.5):
		self.graph = graph
		self.num_ants = num_ants
		self.alpha = alpha # The importance of the pheromone
		self.beta = beta # The importance of the heuristic information
		self.iterations = iterations # The amount of iterations
		self.evaporationRate = evaporationRate # The rate of evaporation
		self.ants = [] # The list of ants

		lista_citys = [city for city in range(0, self.graph.num_vertices)]
		# Creates the ants by putting each one in a city.
		for k in range(self.num_ants):
			city_formiga = random.choice(lista_citys)
			lista_citys.remove(city_formiga)
			self.ants.append(Ant(city=city_formiga))
			if not lista_citys:
				lista_citys = [city for city in range(0, self.graph.num_vertices)]


		# Calculates the greedy cost to use in the pheromone initialization
		cost_greedy = 0.0 # cost greedy
		vertice_initial = random.randint(0, graph.num_vertices-1) # Selects a random vertex
		vertice_current = vertice_initial
		visited = [vertice_current] # The list of visited
		while True:
			neighbors = self.graph.neighbors[vertice_current][:]
			costs, option = [], {}
			for neighbor in neighbors:
				if neighbor not in visited:
					cost = self.graph.getCostEdge(vertice_current, neighbor)
					option[cost] = neighbor
					costs.append(cost)
			if len(visited) == self.graph.num_vertices:
				break
			min_cost = min(costs) # Get the lowest cost list.
			cost_greedy += min_cost # Add the cost to the total
			vertice_current = option[min_cost] # Updates the current vertex
			visited.append(vertice_current) # Marks the current as a visited vertex

		# Add the cost of the last visited of the cost_ greedy
		cost_greedy += self.graph.getCostEdge(visited[-1], vertice_initial)

		# Initializes the pheromone of all edges
		for key_edge in self.graph.edges:
			pheromone = 1.0 / (self.graph.num_vertices * cost_greedy)
			self.graph.setPheromoneEdge(key_edge[0], key_edge[1], pheromone)


	def rotate(self):

		for it in range(self.iterations):

			# List of lists of the city 's visited by each ant.
			citys_visited = []
			for k in range(self.num_ants):
				# Add the city of origin of each ant.
				citys = [self.ants[k].getCity()]
				citys_visited.append(citys)

			# For each ant builds a solution.
			for k in range(self.num_ants):
				for i in range(0, self.graph.num_vertices-1):
					# Get all the neighbors that have not been visited
					citys_not_visited = list(set(self.graph.neighbors[self.ants[k].getCity()]) - set(citys_visited[k]))
					
					# The sum of the number of city 's not visited by ant "k"
					# Will be used in the calculation of the likelihood
					somatorio = 0.0
					for city in citys_not_visited:
						# Calculates the pheromone
						pheromone =  self.graph.getPheromoneEdge(self.ants[k].getCity(), city)
						# Obtains the distance
						distance = self.graph.getCostEdge(self.ants[k].getCity(), city)
						# Add in the sum
						somatorio += (math.pow(pheromone, self.alpha) * math.pow(1.0 / distance, self.beta))

					# probabilities f choosing a path
					probabilities = {}

					for city in citys_not_visited:
						# Calculates the pheromone
						pheromone = self.graph.getPheromoneEdge(self.ants[k].getCity(), city)
						# Obtains the distance
						distance = self.graph.getCostEdge(self.ants[k].getCity(), city)
						#  Obtains the probability
						probability = (math.pow(pheromone, self.alpha) * math.pow(1.0 / distance, self.beta)) / (somatorio if somatorio > 0 else 1)
						# Add to the list of probabilities
						probabilities[city] = probability
						# print(probabilities)

					# Obtains the chosen city 
					city_chosen = max(probabilities, key=probabilities.get) 

					# Adds the chosen city to the list of citys visited by ant "K"
					citys_visited[k].append(city_chosen)

				# Updates the solution found by the ant.
				self.ants[k].setSolution(citys_visited[k], self.graph.getCostPath(citys_visited[k]))

			# updates the amount of pheromone 
			for edge in self.graph.edges:
				# The sum of the pheromones of the edge
				somatorio_pheromone = 0.0
				# For each ant "K"
				for k in range(self.num_ants):
					edges_formiga = []
					# It generates all the edges travelled by the ant "K"
					for j in range(self.graph.num_vertices - 1):#changed
						edges_formiga.append((citys_visited[k][j], citys_visited[k][j+1]))
					# Add the last edge
					edges_formiga.append((citys_visited[k][-1], citys_visited[k][0]))
					# Check whether the edge is part of the path of the ant "K"
					if edge in edges_formiga:
						somatorio_pheromone += (1.0 / self.graph.getCostPath(citys_visited[k]))
				# Calculates the new pheromone
				novo_pheromone = (1.0 - self.evaporationRate) * self.graph.getPheromoneEdge(edge[0], edge[1]) + somatorio_pheromone
				# The arrow of the new pheromone edge
				self.graph.setPheromoneEdge(edge[0], edge[1], novo_pheromone)


		# Through to get the solutions of the ants
		solution, cost = None, None
		for k in range(self.num_ants):
			if not solution:
				solution = self.ants[k].getSolution()[:]
				cost = self.ants[k].getCostSolution()
			else:
				aux_cost = self.ants[k].getCostSolution()
				if aux_cost < cost:
					solution = self.ants[k].getSolution()[:]
					cost = aux_cost
		print('solution final: %s | cost: %d\n' % (' -> '.join(str(i) for i in solution), cost))


if __name__ == "__main__":
	
	# Creates a graph and the number of vertices
	graph = Graph(num_vertices = 52)


	berlin52 = loadtxt('../berlin52.tsp')  
	site={}
	count_0=0
	while(count_0<52):
		site[str(int(count_0))]=[berlin52[count_0][1],berlin52[count_0][2]]
		count_0=count_0+1

	d={}
	count_1=0
	while(count_1<52):
		d[str(int(count_1))]=count_1
		count_1=count_1+1

	def defineDistance(a,b):
		return math.sqrt((site[a][0] - site[b][0]) ** 2 + (site[a][1] - site[b][1]) ** 2)

		

	# Adds the edges
	pointer_i=0
	while(pointer_i<52):
		pointer_j=pointer_i
		while(pointer_j<52):
			graph.addEdge(d[str(int(pointer_i))], d[str(int(pointer_j))],defineDistance(str(int(pointer_i)),str(int(pointer_j))))
			pointer_j=pointer_j+1
		pointer_i=pointer_i+1
			

	
	

	# Creates an instance of the ACO
	
	aco = ACO(graph=graph, num_ants=graph.num_vertices, alpha=1.0, beta=5.0, iterations=1000, evaporationRate=0.5)
	# rotate the algorithm
	aco.rotate()
	
	# Test with graph complete
	'''
	num_vertices = 52
	print('The test graph with %d vertices...\n' % num_vertices)
	graph_complete = GraphComplete(num_vertices=num_vertices)
	graph_complete.generate()
	aco2 = ACO(graph=graph_complete, num_ants=graph_complete.num_vertices, 
				alpha=1, beta=5, iterations=100, evaporationRate=0.5)
	aco2.rotate()
	'''