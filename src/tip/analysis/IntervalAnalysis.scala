package tip.analysis

import tip.cfg._
import tip.ast.AstNodeData.DeclarationData
import tip.lattices.IntervalLattice._
import tip.lattices._
import tip.solvers._

trait IntervalAnalysisWidening extends ValueAnalysisMisc with Dependencies[CfgNode] with FinalWarnings {

  import tip.cfg.CfgOps._

  val cfg: ProgramCfg

  val valuelattice: IntervalLattice.type

  val liftedstatelattice: LiftLattice[statelattice.type]

  /**
    * Int values occurring in the program, plus -infinity and +infinity.
    */
  private val B = cfg.nodes.flatMap { n =>
    n.appearingConstants.map { x =>
      IntNum(x.value): Num
    } + MInf + PInf
  }

  def loophead(n: CfgNode): Boolean = indep(n).exists(cfg.rank(_) > cfg.rank(n))

  private def minB(b: IntervalLattice.Num) = B.filter(b <= _).min

  private def maxB(a: IntervalLattice.Num) = B.filter(_ <= a).max

  def widenInterval(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element =
    (x, y) match {
      case (IntervalLattice.EmptyInterval, _) => y
      case (_, IntervalLattice.EmptyInterval) => x
      case ((l1, h1), (l2, h2)) => (if (l1 <= l2) l1 else maxB(l2), if (h2 <= h1) h1 else minB(h2))
    }

  def widen(x: liftedstatelattice.Element, y: liftedstatelattice.Element): liftedstatelattice.Element =
    (x, y) match {
      case (liftedstatelattice.Bottom, _) => y
      case (_, liftedstatelattice.Bottom) => x
      case (liftedstatelattice.Lift(xm), liftedstatelattice.Lift(ym)) =>
        liftedstatelattice.Lift(declaredVars.map { v =>
          v -> widenInterval(xm(v), ym(v))
        }.toMap)
    }

  /**
    * Used for constraining the interval of identifier in assert X > E
    */
  override def gtAssert(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element = {
    val addOneY = y match {
      case (IntNum(i), _) => IntNum(i+1)
      case _ => y._1
    }
    if (y._1 > x._2) valuelattice.bottom else (addOneY, x._2)
  }

  /**
    * Used for constraining the interval of identifier in assert E > X
    */
  override def leqAssert (x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element = {
    val minusOneY = y match {
      case (_ ,IntNum(i)) => IntNum(i-1)
      case _ => y._2
    }
    if (x._1 > y._2) valuelattice.bottom else (x._1, minusOneY)
  }

  /**
    * Returns true if the interval of the write expression is contained in the interval of the device.
    */
  override def contained(deviceInterval: valuelattice.Element, writeInterval: valuelattice.Element): Boolean =
    if (writeInterval._1 < deviceInterval._1 || writeInterval._2 > deviceInterval._2) false else true
}

/**
  * A mixin for remembering the latest message for each location.
  */
trait FinalWarnings extends ValueAnalysisMisc {
  val warnings = collection.mutable.LinkedHashMap[String, String]()
  override def saveWarning(key: String, msg: String): Unit = {
    warnings(key) = msg
  }
  def printWarnings() = {
    println("Device write analysis results:")
    for ((key,msg) <- warnings) {
      println("  " + key + ": " + msg)
    }
  }
}

object IntervalAnalysis {

  object Intraprocedural {

    /**
      * Interval analysis, using the worklist solver with init and widening.
      */
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with IntervalAnalysisWidening {

      override def analyze(): lattice.Element = {
        val result = super.analyze()
        printWarnings()
        result
      }
    }

    /**
      * Interval analysis, using the worklist solver with init, widening, and narrowing.
      */
    class WorklistSolverWithWideningAndNarrowing(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[CfgNode]
        with IntervalAnalysisWidening {

      override def analyze(): lattice.Element = {
        val result = super.analyze()
        printWarnings()
        result
      }

      val narrowingSteps = 5
    }
  }
}
