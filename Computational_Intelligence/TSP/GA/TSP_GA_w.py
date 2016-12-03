# -*- encoding: utf-8 -*-

import random
import math
import sys

if sys.version_info.major < 3:
      import Tkinter
else:
      import tkinter as Tkinter
      
from GA import GA


class TSP_WIN(object):
      def __init__(self, aRoot, aIndividualCount = 100, aWidth = 560, aHeight = 330):
            self.root = aRoot
            self.individualCount = aIndividualCount
            self.width = aWidth
            self.height = aHeight
            self.canvas = Tkinter.Canvas(
                        self.root,
                        width = self.width,
                        height = self.height,
                  )
            self.canvas.pack(expand = Tkinter.YES, fill = Tkinter.BOTH)
            self.bindEvents()
            self.initCitys()
            self.new()
            self.title("TSP")


      def initCitys(self):
            self.citys = []

            # import the data set of TSP
            a = loadtxt('berlin52.tsp')  
            self.citys = a[:,1:]

            #坐标变换
            minX, minY = self.citys[0][0], self.citys[0][1]
            maxX, maxY = minX, minY
            for city in self.citys[1:]:
                  if minX > city[0]:
                        minX = city[0]
                  if minY > city[1]:
                        minY = city[1]
                  if maxX < city[0]:
                        maxX = city[0]
                  if maxY < city[1]:
                        maxY = city[1]

            w = maxX - minX
            h = maxY - minY
            xoffset = 30
            yoffset = 30
            ww = self.width - 2 * xoffset
            hh = self.height - 2 * yoffset
            xx = ww / float(w)
            yy = hh / float(h)
            r = 5
            self.nodes = []
            self.nodes2 = []
            for city in self.citys:
                  x = (city[0] - minX ) * xx + xoffset
                  y = hh - (city[1] - minY) * yy + yoffset
                  self.nodes.append((x, y))
                  node = self.canvas.create_oval(x - r, y -r, x + r, y + r,
                        fill = "#ff0000",
                        outline = "#000000",
                        tags = "node",)
                  self.nodes2.append(node)

            

            
      def distance(self, order):
            distance = 0.0
            for i in range(-1, len(self.citys) - 1):
                  index1, index2 = order[i], order[i + 1]
                  city1, city2 = self.citys[index1], self.citys[index2]
                  distance += math.sqrt((city1[0] - city2[0]) ** 2 + (city1[1] - city2[1]) ** 2)
            return distance


      def matchFun(self):
            return lambda individual: 1.0 / self.distance(individual.gene)


      def title(self, text):
            self.root.title(text)


      def line(self, order):
            self.canvas.delete("line") 
            for i in range(-1, len(order) -1):
                  p1 = self.nodes[order[i]]
                  p2 = self.nodes[order[i + 1]]
                  self.canvas.create_line(p1, p2, fill = "#000000", tags = "line")
 


      def bindEvents(self):
            self.root.bind("n", self.new)
            self.root.bind("g", self.start)
            self.root.bind("s", self.stop)


      def new(self, evt = None):
            self.isRunning = False
            order = range(len(self.citys))
            self.line(order)
            self.ga = GA(aCrossRate = 0.7, 
                  aMutationRage = 0.02, 
                  aIndividualCount = self.individualCount, 
                  aGeneLenght = len(self.citys), 
                  aMatchFun = self.matchFun())


      def start(self, evt = None):
            self.isRunning = True
            while self.isRunning:
                  self.ga.next()
                  distance = self.distance(self.ga.best.gene)
                  self.line(self.ga.best.gene)
                  self.title("TSP-gen: %d" % self.ga.generation)
                  self.canvas.update()


      def stop(self, evt = None):
            self.isRunning = False


      def mainloop(self):
            self.root.mainloop()


def main():
      #tsp = TSP()
      #tsp.run(10000)

      tsp = TSP_WIN(Tkinter.Tk())
      tsp.mainloop()


if __name__ == '__main__':
      main()