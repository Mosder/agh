from copy import deepcopy
from dimacs import loadWeightedGraph
from graphClass import Graph
from lab2 import getMaxFlow, edgeListToAdjMatrix
from math import inf
from tester import Tester
from time import time

def getEdgeConnectivity(graph: Graph):
    fileLoadData = loadWeightedGraph(graph.dir + graph.name)
    vAmount = len(graph.data[0])
    edgeConnectivity = inf
    for i in range(1, vAmount):
        edgeConnectivity = min(edgeConnectivity, getMaxFlow(graph, endNode=i))
        graph.data = edgeListToAdjMatrix(*fileLoadData, directed=False)
    return edgeConnectivity

def loadGraph(graphPath):
    return edgeListToAdjMatrix(*loadWeightedGraph(graphPath), directed=False)

if __name__ == "__main__":
    tester = Tester("graphs/lab3/", loadGraph, getEdgeConnectivity)
    tester.continousTesting()