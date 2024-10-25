from dimacs import readSolution
from os.path import isfile, isdir
from os import listdir
from graphClass import Graph
from time import time

class Tester:
    def __init__(self, graphDir, loadFunction, testFunction, *remainingTestFunctionArgs):
        self.graphDir = graphDir
        self.loadFunction = loadFunction
        self.testFunction = testFunction
        self.remainingTestFunctionArgs = remainingTestFunctionArgs
    
    def changeRemainingTestFunctionArgs(self, *remainingTestFunctionArgs):
        self.remainingTestFunctionArgs = remainingTestFunctionArgs

    def printResult(self, result, graphName, timeTaken):
        expected = int(readSolution(self.graphDir + graphName))
        print(f"Graph: {graphName}")
        print(f"Expected: {expected}")
        print(f"Result: {result} ({'ok' if result == expected else 'wrong'})")
        print(f"Time: {'{:.3f}'.format(timeTaken)}s")
    
    def isOk(self, result, graphName):
        return int(readSolution(self.graphDir + graphName)) == result

    def testGraph(self, graphName):
        graphPath = self.graphDir + graphName
        t0 = time()
        if isfile(graphPath):
            data = self.loadFunction(graphPath)
            graph = Graph(self.graphDir, graphName, data)
            result = self.testFunction(graph, *self.remainingTestFunctionArgs)
            timeTaken = time() - t0
            self.printResult(result, graphName, timeTaken)
            return [self.isOk(result, graphName), timeTaken]
        else:
            print(f"Graph {graphName} ({graphPath}) doesn't exist")
        return [False, time() - t0]

    def testAllGraphs(self):
        if isdir(self.graphDir):
            testsPassed = 0
            totalTime = 0
            dir = listdir(self.graphDir)
            for index, graphName in enumerate(dir):
                print(f"Test {index+1}:")
                testData = self.testGraph(graphName)
                if testData[0]: testsPassed += 1
                totalTime += testData[1]
                print()
            print(f"Test passsed: {testsPassed} / {len(dir)}")
            print(f"Total time: {'{:.3f}'.format(totalTime)}s")
        else:
            print(f"Directory {self.graphDir} doesn't exist")

    def continousTesting(self):
        inp = input("Graph name / all (:q to quit): ")
        while inp != ":q":
            if inp == "all":
                self.testAllGraphs()
            else:
                self.testGraph(inp)
            inp = input("Graph name / all (:q to quit): ")

    