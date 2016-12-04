# -*- coding: utf-8 -*-
# SONG DAIWEI 
import random
from Individual import Individual

class GA(object):
      """class of GA"""
      def __init__(self, aCrossRate, aMutationRage, aIndividualCount, aGeneLenght, aMatchFun = lambda individual : 1):
            self.crossRate = aCrossRate
            self.mutationRate = aMutationRage
            self.individualCount = aIndividualCount
            self.geneLenght = aGeneLenght
            self.matchFun = aMatchFun                 # adaptation function
            self.population = []                           # population
            self.best = None                          # elitist selection
            self.generation = 1
            self.crossCount = 0
            self.mutationCount = 0
            self.bounds = 0.0                         # sum of adaptation to calculate the probability when selection

            self.initPopulation()


      def initPopulation(self):
            """initial the population"""
            self.population = []
            for i in range(self.individualCount):
                  gene = [ x for x in range(self.geneLenght) ] 
                  random.shuffle(gene)
                  individual = Individual(gene)
                  self.population.append(individual)


      def judge(self):
            """judge to calculate the adaptation of each individual"""
            self.bounds = 0.0
            self.best = self.population[0]
            for individual in self.population:
                  individual.score = self.matchFun(individual)
                  self.bounds += individual.score
                  if self.best.score < individual.score:
                        self.best = individual


      def cross(self, parent1, parent2):
            """cross"""
            index1 = random.randint(0, self.geneLenght - 1)
            index2 = random.randint(index1, self.geneLenght - 1)
            tempGene = parent2.gene[index1:index2]   # cross the pieces of genes
            newGene = []
            p1len = 0
            for g in parent1.gene:
                  if p1len == index1:
                        newGene.extend(tempGene)     # insert the pieces of genes
                        p1len += 1
                  if g not in tempGene:
                        newGene.append(g)
                        p1len += 1
            self.crossCount += 1
            return newGene


      def  mutation(self, gene):
            """mutation of gene"""
            index1 = random.randint(0, self.geneLenght - 1)
            index2 = random.randint(0, self.geneLenght - 1)

            newGene = gene[:]       # generate a new sequence of gene in order not to infect the father population when it mutates # Yvon-Shong 
            newGene[index1], newGene[index2] = newGene[index2], newGene[index1]    # swap
            self.mutationCount += 1
            return newGene


      def getOne(self):
            """select one Individual"""
            r = random.uniform(0, self.bounds)    # generate a real number, the threhold of evolution
            for individual in self.population:
                  r -= individual.score
                  if r <= 0:
                        return individual

            raise Exception("Wrong selection", self.bounds)


      def newChild(self):
            """generate the child"""
            parent1 = self.getOne()
            

            # cross by the probability
            rate1 = random.random()
            if rate1 < self.crossRate:
                  # cross
                  parent2 = self.getOne()
                  gene = self.cross(parent1, parent2)
            else:
                  gene = parent1.gene

            # mutate by the probability
            rate2 = random.random()
            if rate2 < self.mutationRate:
                  gene = self.mutation(gene)

            return Individual(gene)


      def next(self):
            """generate the next generation"""
            self.judge()
            newPopulation = []
            newPopulation.append(self.best)  # elitist selection, choose the best ONE to add to the next generation
            while len(newPopulation) < self.individualCount:
                  newPopulation.append(self.newChild())
            self.population = newPopulation
            self.generation += 1
		