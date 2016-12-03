# -*- coding: utf-8 -*-

import random
from Individual import Individual

class GA(object):
      """遗传算法类"""
      def __init__(self, aCrossRate, aMutationRage, aIndividualCount, aGeneLenght, aMatchFun = lambda individual : 1):
            self.crossRate = aCrossRate
            self.mutationRate = aMutationRage
            self.individualCount = aIndividualCount
            self.geneLenght = aGeneLenght
            self.matchFun = aMatchFun                 # 适配函数
            self.lives = []                           # 种群
            self.best = None                          # 保存这一代中最好的个体
            self.generation = 1
            self.crossCount = 0
            self.mutationCount = 0
            self.bounds = 0.0                         # 适配值之和，用于选择时计算概率

            self.initPopulation()


      def initPopulation(self):
            """初始化种群"""
            self.lives = []
            for i in range(self.individualCount):
                  gene = [ x for x in range(self.geneLenght) ] 
                  random.shuffle(gene)
                  individual = Individual(gene)
                  self.lives.append(individual)


      def judge(self):
            """评估，计算每一个个体的适配值"""
            self.bounds = 0.0
            self.best = self.lives[0]
            for individual in self.lives:
                  individual.score = self.matchFun(individual)
                  self.bounds += individual.score
                  if self.best.score < individual.score:
                        self.best = individual


      def cross(self, parent1, parent2):
            """交叉"""
            index1 = random.randint(0, self.geneLenght - 1)
            index2 = random.randint(index1, self.geneLenght - 1)
            tempGene = parent2.gene[index1:index2]   # 交叉的基因片段
            newGene = []
            p1len = 0
            for g in parent1.gene:
                  if p1len == index1:
                        newGene.extend(tempGene)     # 插入基因片段
                        p1len += 1
                  if g not in tempGene:
                        newGene.append(g)
                        p1len += 1
            self.crossCount += 1
            return newGene


      def  mutation(self, gene):
            """突变"""
            index1 = random.randint(0, self.geneLenght - 1)
            index2 = random.randint(0, self.geneLenght - 1)

            newGene = gene[:]       # 产生一个新的基因序列，以免变异的时候影响父种群
            newGene[index1], newGene[index2] = newGene[index2], newGene[index1]    # swap
            self.mutationCount += 1
            return newGene


      def getOne(self):
            """选择一个个体"""
            r = random.uniform(0, self.bounds)    # generate a real number, the threhold of evolution
            for individual in self.lives:
                  r -= individual.score
                  if r <= 0:
                        return individual

            raise Exception("选择错误", self.bounds)


      def newChild(self):
            """产生新后代"""
            parent1 = self.getOne()
            

            # 按概率交叉
            rate1 = random.random()
            if rate1 < self.crossRate:
                  # 交叉
                  parent2 = self.getOne()
                  gene = self.cross(parent1, parent2)
            else:
                  gene = parent1.gene

            # 按概率突变
            rate2 = random.random()
            if rate2 < self.mutationRate:
                  gene = self.mutation(gene)

            return Individual(gene)


      def next(self):
            """产生下一代"""
            self.judge()
            newLives = []
            newLives.append(self.best)            #精英选择 把最好的1个个体加入下一代
            while len(newLives) < self.individualCount:
                  newLives.append(self.newChild())
            self.lives = newLives
            self.generation += 1
		