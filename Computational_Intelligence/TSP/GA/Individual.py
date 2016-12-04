# -*- encoding: utf-8 -*-
# SONG DAIWEI 

SCORE_NONE = -1

class Individual(object):
      """class of Individual"""
      def __init__(self, aGene = None):
            self.gene = aGene
            self.score = SCORE_NONE


