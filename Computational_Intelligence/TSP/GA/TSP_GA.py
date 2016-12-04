# -*- encoding: utf-8 -*-
# SONG DAIWEI 
import random
import math
from numpy  import *
from GA import GA

class TSP(object):
      def __init__(self, aIndividualCount = 100,):
            self.initCitys()
            self.individualCount = aIndividualCount
            self.ga = GA(aCrossRate = 0.7, 
                  aMutationRage = 0.05, 
                  aIndividualCount = self.individualCount, 
                  aGeneLenght = len(self.citys), 
                  aMatchFun = self.matchFun())


      def initCitys(self):
            self.citys = []
            """
            for i in range(34):
                  x = random.randint(0, 1000)
                  y = random.randint(0, 1000)
                  self.citys.append((x, y))
            """
            # import the data set of TSP
            a = loadtxt('../berlin52.tsp')  
            self.citys = a[:,1:]
            
      
      
      
      def distance(self, order):
            distance = 0.0
            for i in range(-1, len(self.citys) - 1): # -1 ???
                  index1, index2 = order[i], order[i + 1]
                  city1, city2 = self.citys[index1], self.citys[index2]
                  distance += math.sqrt((city1[0] - city2[0]) ** 2 + (city1[1] - city2[1]) ** 2)

                  """
                  R = 6371.004
                  Pi = math.pi 
                  LatA = city1[1]
                  LatB = city2[1]
                  MLonA = city1[0]
                  MLonB = city2[0]

                  C = math.sin(LatA*Pi / 180) * math.sin(LatB * Pi / 180) + math.cos(LatA * Pi / 180) * math.cos(LatB * Pi / 180) * math.cos((MLonA - MLonB) * Pi / 180)
                  D = R * math.acos(C) * Pi / 100
                  distance += D
                  """
            
            return distance


      def matchFun(self):
            return lambda individual: 1.0 / self.distance(individual.gene)   # define nonymous function


      def run(self, n = 0):
            while n > 0:
                  self.ga.next()
                  distance = self.distance(self.ga.best.gene)
                  if (n-2) %100   == 0 :
                        print (("%d : %f") % (self.ga.generation, distance))
                  n -= 1


def main():
      tsp = TSP()
      tsp.run(1000000000)
      


if __name__ == '__main__':
      main()


