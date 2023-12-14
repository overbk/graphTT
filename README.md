## graphTT

This repository contains a Scala implementation of a termination method for PBPO+ graph transformation systems, described in a recent research paper:

R. Overbeek & J. Endrullis. Termination of Graph Transformation Systems Using Weighted Subgraph Counting. arXiv, 2023. [url](todo)

### Context

The [algebraic approach to graph transformation](https://en.wikipedia.org/wiki/Graph_rewriting#Algebraic_approach) uses category theory to define graph transformation systems. PBPO+ ([Overbeek et al., 2023](https://www.sciencedirect.com/science/article/pii/S2352220823000275)) is a recent proposal that unifies several existing approaches in the setting of quasitoposes. Roughly speaking, a quasitopos is a category that is set-like in a variety of ways (e.g., in the way unions behave), and it contains many graph categories of interest.

A fundamental question for graph transformation, and computational models in general, is when and how one can decide whether a given system terminates, i.e., whether it admits only finite computations on arbitrary input. Criteria for graph transformation are sparse, and usually defined for specific graph notions. The method proposed in ([Overbeek & Endrullis, 2023](https://arxiv.org/abs/2303.07812)) is instead defined more abstractly for more general categories; more precisely, the rm-adhesive quasitoposes.

When implementing such a categorically formulated method, one may wonder how many of the categorical constructions can carry over into program code. [Computational category theory](https://www.epatters.org/wiki/algebra/computational-category-theory.html) tries to implement the categorical constructions as faithfully as possible.

This repository follows the computational category theory philosophy. Performance considerations have been considered to a limited extent only.

### Code outline

The source directory is structured as follows:
  - Directory *categorytheory* contains all basic definitions related to general categories, as well as more specialized classes of categories such as presheaf categories and quasitoposes.
  - Directory *labeledgraph* contains a definition of the category of labeled graphs, which is an instance of a topos, and thus of an rm-adhesive quasitopos.
  - Directory *rewriting* contains definitions and examples of PBPO+ rules (as well as DPO rules).
  - Directory *termination* implements the termination method, as well as formatting functions.
  - Directory *repl*, *parsing*, and *util* speak for themselves.

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

When run, the graphTT REPL starts. For an explanation of this REPL, see the research paper cited above.
