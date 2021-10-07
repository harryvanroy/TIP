package tip.analysis

import tip.cfg.CfgOps._
import tip.cfg.{CfgNode, CfgStmtNode, ProgramCfg}
import tip.lattices.{MapLattice, DisconnectLattice}
import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.solvers.FixpointSolvers

import scala.collection.immutable.Set

/**
  * Simple intra-procedural disconnect analysis.
  * Inspired by SimplySignAnalysis.scala.
  */
class DisconnectAnalysis(cfg: ProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg)  {

  /**
    * The lattice of abstract values.
    */
  val valuelattice = DisconnectLattice

  /**
    * Set of declared variables, used by `statelattice`.
    */
  val declaredVars: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVarsAndParams)

  /**
    * The lattice of abstract states.
    */
  val statelattice: MapLattice[ADeclaration, DisconnectLattice.type] = new MapLattice(declaredVars, valuelattice)

  /**
    * The program lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(domain, statelattice)

  /**
    * Abstract evaluation of expressions.
    */
  def eval(exp: AExpr, env: statelattice.Element)(implicit declData: DeclarationData): valuelattice.Element = {
    import valuelattice._
    exp match {
      case device: ADevice => notDisconnected
      case _ => valuelattice.bottom
    }
  }

  /**
    * Incoming dependencies. Used when computing the join from predecessors.
    * @param n an element from the worklist
    * @return the elements that the given element depends on
    */
  def indep(n: CfgNode): Set[CfgNode] = n.pred.toSet

  /**
    * Transfer functions for the different kinds of statements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
    NoPointers.assertContainsNode(n.data)
    NoCalls.assertContainsNode(n.data)
    NoRecords.assertContainsNode(n.data)
    n match {
      case r: CfgStmtNode =>
        r.data match {
          // var declarations
          case varr: AVarStmt => s ++ (for (v<-varr.declIds) yield (v -> DisconnectLattice.bottom))

          // assignments
          case AAssignStmt(id: AIdentifier, right, _) => s + (declData(id) -> eval(right, s))
          case ADeviceDisconnect(device: AIdentifier, _) => s + (declData(device) -> DisconnectLattice.isDisconnected)
          // all others: like no-ops
          case _ => s
        }
      case _ => s
    }
  }

  /**
    * The constraint function for individual elements in the map domain.
    * First computes the join of the incoming elements and then applies the transfer function.
    * @param n the current location in the map domain
    * @param x the current lattice element for all locations
    * @return the output sublattice element
    */
  def funsub(n: CfgNode, x: lattice.Element): lattice.sublattice.Element =
    localTransfer(n, join(n, x))

  /**
    * Computes the least upper bound of the incoming elements.
    */
  def join(n: CfgNode, o: lattice.Element): lattice.sublattice.Element = {
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))
  }

  /**
    * The function for which the least fixpoint is to be computed.
    * Applies the sublattice constraint function pointwise to each entry.
    * @param x the input lattice element
    * @return the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element = {
    FixpointSolvers.log.verb(s"In state $x")
    domain.foldLeft(lattice.bottom)(
      (m, a) =>
        m + (a -> {
          FixpointSolvers.log.verb(s"Processing $a")
          funsub(a, x)
        })
    )
  }

  /**
    * The basic Kleene fixpoint solver.
    */
  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var t = x
    do {
      t = x
      x = fun(x)
    } while (x != t)
    printWarnings(x)
    x
  }

  /**
    * Print warning messages for disconnect analysis.
    */
  def printWarnings(x: lattice.Element) = {
    println("Disconnect analysis results:")
    for ((cfgNode, stateLatticeVal) <- x) {
      cfgNode.data match {
        case returnStmt: AReturnStmt => {
          for ((decl, discLatticeVal) <- stateLatticeVal) {
            if (discLatticeVal == valuelattice.top || discLatticeVal == DisconnectLattice.notDisconnected) {
              println(s"WARNING: ${decl} device may not have been disconnected.")
            }
          }
        }
        case _ =>
      }
    }
  }
}