package com.pbpoplus.rewriting

import com.pbpoplus.labeledgraph.{LabeledGraph, LabeledGraphMorphism}

type LabeledGraphPbpoPlusRule[T] = 
  PbpoPlusRule[LabeledGraph[T, T, T, T], LabeledGraphMorphism[T, T, T, T, T, T]]
