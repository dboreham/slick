package scala.slick.compiler

import scala.math.{min, max}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.slick.SlickException
import scala.slick.ast._
import Util._
import ExtraUtil._
import TypeUtil._

/** Rewrite zip joins into a form suitable for SQL (using inner joins and
  * RowNumber columns.
  * We rely on having a Bind around every Join and both of its generators,
  * which should have been generated by Phase.forceOuterBinds. The inner
  * Binds need to select Pure(StructNode(...)) which should be the outcome
  * of Phase.rewritePaths. */
class ResolveZipJoins extends Phase {
  type State = ResolveZipJoinsState
  val name = "resolveZipJoins"

  def apply(state: CompilerState) = {
    val n2 = ClientSideOp.mapServerSide(state.tree, true)(resolveZipJoins)
    state + (this -> new State(n2 ne state.tree)) withNode n2
  }

  def resolveZipJoins(n: Node): Node = (n match {
    // zip with index
    case Bind(oldBindSym, Join(_, _,
        l @ Bind(lsym, lfrom, Pure(StructNode(lstruct))),
        RangeFrom(offset),
        JoinType.Zip, LiteralNode(true)), Pure(sel)) =>
      val idxSym = new AnonSymbol
      val idxExpr =
        if(offset == 1L) RowNumber()
        else Library.-.typed[Long](RowNumber(), LiteralNode(1L - offset))
      val innerBind = Bind(lsym, lfrom, Pure(StructNode(lstruct :+ (idxSym, idxExpr))))
      val bindSym = new AnonSymbol
      val OldBindRef = Ref(oldBindSym)
      val newOuterSel = sel.replace {
        case Select(OldBindRef, ElementSymbol(1)) => Ref(bindSym)
        case Select(OldBindRef, ElementSymbol(2)) => Select(Ref(bindSym), idxSym)
      }
      Bind(bindSym, innerBind, Pure(newOuterSel)).nodeWithComputedType(SymbolScope.empty, false, true)

    // zip with another query
    case b @ Bind(_, Join(jlsym, jrsym,
        l @ Bind(lsym, lfrom, Pure(StructNode(lstruct))),
        r @ Bind(rsym, rfrom, Pure(StructNode(rstruct))),
        JoinType.Zip, LiteralNode(true)), _) =>
      val lIdxSym, rIdxSym = new AnonSymbol
      val lInnerBind = Bind(lsym, lfrom, Pure(StructNode(lstruct :+ (lIdxSym, RowNumber()))))
      val rInnerBind = Bind(rsym, rfrom, Pure(StructNode(rstruct :+ (rIdxSym, RowNumber()))))
      val join = Join(jlsym, jrsym, lInnerBind, rInnerBind, JoinType.Inner,
        Library.==.typed[Boolean](Select(Ref(jlsym), lIdxSym), Select(Ref(jrsym), rIdxSym))
      )
      b.copy(from = join).nodeWithComputedType(SymbolScope.empty, false, true)

    case n => n
  }).nodeMapChildren(resolveZipJoins, keepType = true)
}

class ResolveZipJoinsState(val hasRowNumber: Boolean)

/** Conversion of basic ASTs to a shape suitable for relational DBs.
  * This phase replaces all nodes of types Bind, Filter, SortBy, Take and Drop
  * by Comprehension nodes and merges nested Comprehension nodes. */
class ConvertToComprehensions extends Phase {
  val name = "convertToComprehensions"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(convert.repeat)
  }

  val convert = new Transformer {
    override val keepType = true

    def mkFrom(s: Symbol, n: Node): Seq[(Symbol, Node)] = n match {
      case Pure(ProductNode(Seq())) => Seq.empty
      case n => Seq((s, n))
    }

    def replace = {
      // GroupBy to Comprehension
      case b @ Bind(gen, gr @ GroupBy(fromGen, from, by), Pure(sel)) =>
        convertSimpleGrouping(gen, gr.nodeType.asCollectionType.elementType,
          fromGen, from, by, sel).nodeTyped(b.nodeType)
      case g: GroupBy =>
        throw new SlickException("Unsupported query shape containing .groupBy without subsequent .map")
      // Bind to Comprehension
      case b @ Bind(gen, from, select) =>
        Comprehension(from = mkFrom(gen, from), select = Some(select)).nodeTyped(b.nodeType)
      // Filter to Comprehension
      case f @ Filter(gen, from, where) =>
        Comprehension(from = mkFrom(gen, from), where = Seq(where)).nodeTyped(f.nodeType)
      // SortBy to Comprehension
      case s @ SortBy(gen, from, by) =>
        Comprehension(from = mkFrom(gen, from), orderBy = by).nodeTyped(s.nodeType)
      // Take and Drop to Comprehension
      case td @ TakeDrop(from, take, drop, gen) =>
        val drop2 = if(drop == Some(0)) None else drop
        val c =
          if(take == Some(0)) Comprehension(from = mkFrom(gen, from), where = Seq(LiteralNode(false)))
          else Comprehension(from = mkFrom(gen, from), fetch = take.map(_.toLong), offset = drop2.map(_.toLong))
        c.nodeTyped(td.nodeType)
      // Merge Comprehension which selects another Comprehension
      case c1 @ Comprehension(from1, where1, None, orderBy1,
          Some(c2 @ Comprehension(from2, where2, None, orderBy2, select, None, None)),
          fetch, offset) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2,
          orderBy = orderBy2 ++ orderBy1, fetch = fetch, offset = offset
        ).nodeTyped(c1.nodeType)
    }
  }

  /** Convert a GroupBy followed by an aggregating map operation to a Comprehension */
  def convertSimpleGrouping(gen: Symbol, genType: Type, fromGen: Symbol, from: Node, by: Node, sel: Node): Node = {
    val newBy = by.replace({ case r @ Ref(f) if f == fromGen => Ref(gen).nodeTyped(r.nodeType) }, keepType = true)
    val newSel = sel.replace({
      case a @ Apply(fs, Seq(b @ Bind(s1, Select(Ref(gen2), ElementSymbol(2)), Pure(ProductOfCommonPaths(s2, rests)))))
        if (s2 == s1) && (gen2 == gen) =>
        Apply(if(fs == Library.CountAll) Library.Count else fs, Seq(FwdPath(gen :: rests.head).nodeTyped(b.nodeType)))(a.nodeType)
      case ca @ Library.CountAll(Select(Ref(gen2), ElementSymbol(2))) if gen2 == gen =>
        Library.Count.typed(ca.nodeType, LiteralNode(1))
      case FwdPath(gen2 :: ElementSymbol(idx) :: rest) if gen2 == gen && (idx == 1 || idx == 2) =>
        Phase.fuseComprehensions.select(rest, if(idx == 2) Ref(gen) else newBy)(0)
    }, keepType = true)
    Comprehension(Seq(gen -> from), groupBy = Some(newBy),
      select = Some(Pure(newSel).withComputedTypeNoRec))
  }

  /** An extractor for nested Take and Drop nodes */
  object TakeDrop {
    def unapply(n: Node): Option[(Node, Option[Int], Option[Int], Symbol)] = n match {
      case Take(from, num, sym) => unapply(from) match {
        case Some((f, Some(t), d, _)) => Some((f, Some(min(t, num)), d, sym))
        case Some((f, None, d, _)) => Some((f, Some(num), d, sym))
        case _ => Some((from, Some(num), None, sym))
      }
      case Drop(from, num, sym) => unapply(from) match {
        case Some((f, Some(t), None, _)) => Some((f, Some(max(0, t-num)), Some(num), sym))
        case Some((f, None, Some(d), _)) => Some((f, None, Some(d+num), sym))
        case Some((f, Some(t), Some(d), _)) => Some((f, Some(max(0, t-num)), Some(d+num), sym))
        case _ => Some((from, None, Some(num), sym))
      }
      case _ => None
    }
  }
}

class LiftAggregates extends Phase {
  val name = "liftAggregates"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(lift)
  }

  def lift(n: Node): Node = (n match {
    case c: Comprehension =>
      liftAggregates(createSelect(c)).nodeWithComputedType(SymbolScope.empty, false, true)
    case n => n
  }).nodeMapChildren(lift, keepType = true)

  /** Lift aggregates of sub-queries into the 'from' list or inline them
    * (if they would refer to unreachable symbols when used in 'from'
    * position). */
  def liftAggregates(c: Comprehension): Comprehension = {
    val lift = ArrayBuffer[(AnonSymbol, AnonSymbol, Library.AggregateFunctionSymbol, Comprehension)]()
    val seenGens = HashMap[Symbol, Node]()
    def tr(n: Node): Node = n match {
      //TODO Once we can recognize structurally equivalent sub-queries and merge them, c2 could be a Ref
      case ap @ Apply(s: Library.AggregateFunctionSymbol, Seq(c2: Comprehension)) =>
        if(hasRefToOneOf(c2, seenGens.keySet)) {
          logger.debug("Seen reference to one of {"+seenGens.keys.mkString(", ")+"} in "+c2+" -- inlining")
          // This could still produce illegal SQL code if the reference is nested within another
          // sub-query somewhere in 'from' position. Not much we can do about this though.
          s match {
            case Library.CountAll =>
              if(c2.from.isEmpty) Library.Cast.typed(ap.nodeType, LiteralNode(1))
              else c2.copy(select = Some(Pure(ProductNode(Seq(Library.Count.typed(ap.nodeType, LiteralNode(1)))))))
            case s =>
              val c3 = ensureStruct(c2).nodeWithComputedType(SymbolScope.empty, false, true)
              // All standard aggregate functions operate on a single column
              val Some(Pure(StructNode(Seq((_, expr))))) = c3.select
              val elType = c3.nodeType.asCollectionType.elementType
              c3.copy(select = Some(Pure(ProductNode(Seq(Apply(s, Seq(expr))(elType))))))
          }
        } else {
          val a = new AnonSymbol
          val f = new AnonSymbol
          lift += ((a, f, s, c2))
          Select(Ref(a), f)
        }
      case c: Comprehension => c // don't recurse into sub-queries
      case n => n.nodeMapChildren(tr, keepType = true)
    }
    val c2 = c.nodeMapScopedChildren {
      case (Some(gen), ch) =>
        seenGens += gen -> ch
        ch
      case (None, ch) => tr(ch)
    }
    if(lift.isEmpty) c2
    else {
      val newFrom = lift.map { case (a, f, s, c2) =>
        val a2 = new AnonSymbol
        val (c2b, call) = s match {
          case Library.CountAll =>
            (c2, Library.Count.typed(c2.nodeType.asCollectionType.elementType, LiteralNode(1)))
          case s =>
            val c3 = ensureStruct(c2).nodeWithComputedType(SymbolScope.empty, false, true)
            // All standard aggregate functions operate on a single column
            val Some(Pure(StructNode(Seq((f2, _))))) = c3.select
            val elType = c3.nodeType.asCollectionType.elementType
            (c3, Apply(s, Seq(Select(Ref(a2), f2)))(elType))
        }
        a -> Comprehension(from = Seq(a2 -> c2b),
          select = Some(Pure(StructNode(IndexedSeq(f -> call)))))
      }
      logger.debug("Introducing new generator(s) "+newFrom.map(_._1).mkString(", ")+" for aggregations")
      c2.copy(from = c.from ++ newFrom)
    }
  }

  /** Rewrite a Comprehension to always return a StructNode */
  def ensureStruct(c: Comprehension): Comprehension = {
    c.select match {
      case Some(Pure(_: StructNode)) => c
      case Some(Pure(ProductNode(ch))) =>
        val selStr = {
          val n = StructNode(ch.iterator.map(n => (new AnonSymbol) -> n).toIndexedSeq)
          if(n.nodeChildren.exists(_.nodeType == UnassignedType)) n
          else n.withComputedTypeNoRec
        }
        c.copy(select = Some(Pure(selStr)))
      case Some(Pure(n)) =>
        c.copy(select = Some(Pure(StructNode(IndexedSeq((new AnonSymbol) -> n)))))
      case _ =>
        throw new SlickException("Unexpected Comprehension shape in "+c)
    }
  }

  /** Create a select for a Comprehension without one. */
  def createSelect(c: Comprehension): Comprehension = if(c.select.isDefined) c else {
    c.from.last match {
      case (sym, UnionLeft(Comprehension(_, _, _, _, Some(Pure(StructNode(struct))), _, _))) =>
        val r = Ref(sym)
        val copyStruct = StructNode(struct.map { case (field, _) =>
          (field, Select(r, field))
        })
        c.copy(select = Some(Pure(copyStruct))).nodeWithComputedType(SymbolScope.empty, false, true)
      /*case (sym, Pure(StructNode(struct))) =>
        val r = Ref(sym)
        val copyStruct = StructNode(struct.map { case (field, _) =>
          (field, Select(r, field))
        })
        c.copy(select = Some(Pure(copyStruct)))*/
      case _ => c
    }
  }
}

/** Fuse sub-comprehensions into their parents. */
class FuseComprehensions extends Phase {
  val name = "fuseComprehensions"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(fuse)
  }

  def fuse(n: Node): Node = n.nodeMapChildren(fuse, keepType = true) match {
    case c: Comprehension =>
      logger.debug("Checking:",c)
      val fused = c match {
        case c: Comprehension if isFuseableOuter(c) => fuseComprehension(c)
        case c => c
      }
      fused.nodeWithComputedType(SymbolScope.empty, false, true)
    case n => n
  }

  /** Check if a comprehension allow sub-comprehensions to be fused.
    * This is the case if it has a select clause and not more than one
    * sub-comprehension with a groupBy clause. */
  def isFuseableOuter(c: Comprehension): Boolean = c.select.isDefined &&
    c.from.collect { case (_, c: Comprehension) if c.groupBy.isDefined => 0 }.size <= 1

  /** Check if a Comprehension should be fused into its parent. This happens
    * in the following cases:
    * - It has a Pure generator.
    * - It does not have any generators.
    * - The Comprehension has a 'select' clause which consists only of Paths,
    *   constant values and client-side type conversions.
    * - It refers to a symbol introduced in a previous FROM clause of an outer
    *   Comprehension. */
  def isFuseableInner(sym: Symbol, c: Comprehension, prevSyms: scala.collection.Set[Symbol]): Boolean = {
    logger.debug("Checking for fuseable inner "+sym+" with previous generators: "+prevSyms.mkString(", "))
    def isFuseableColumn(n: Node): Boolean = n match {
      case Path(_) => true
      case _: LiteralNode => true
      case GetOrElse(ch, _) => isFuseableColumn(ch)
      case OptionApply(ch) => isFuseableColumn(ch)
      case _ => false
    }
    c.fetch.isEmpty && c.offset.isEmpty && {
      c.from.isEmpty || c.from.exists {
        case (sym, Pure(_)) => true
        case _ => false
      } || (c.select match {
        case Some(Pure(ProductNode(ch))) =>
          ch.map(isFuseableColumn).forall(identity)
        case _ => false
      }) || hasRefToOneOf(c, prevSyms)
    }
  }

  /** Check if two comprehensions can be fused (assuming the outer and inner
    * comprehension have already been deemed fuseable on their own). */
  def isFuseable(outer: Comprehension, inner: Comprehension): Boolean =
    if(!inner.orderBy.isEmpty || inner.groupBy.isDefined) {
      // inner has groupBy or orderBy
      // -> do not allow another groupBy or where on the outside
      // Further orderBy clauses are allowed. They can be fused with the inner ones.
      outer.groupBy.isEmpty && outer.where.isEmpty
    } else true

  /** Fuse simple Comprehensions (no orderBy, fetch or offset), which are
    * contained in the 'from' list of another Comprehension, into their
    * parent. */
  def fuseComprehension(c: Comprehension): Comprehension = {
    var newFrom = new ArrayBuffer[(Symbol, Node)]
    val newWhere = new ArrayBuffer[Node]
    val newGroupBy = new ArrayBuffer[Node]
    val newOrderBy = new ArrayBuffer[(Node, Ordering)]
    val structs = new HashMap[Symbol, Node]
    var fuse = false

    def inline(n: Node): Node = n match {
      case p @ FwdPath(syms) =>
        logger.debug("Inlining "+FwdPath.toString(syms)+" with structs "+structs.keySet)
        structs.get(syms.head).map{ base =>
          logger.debug("  found struct "+base)
          val repl = select(syms.tail, base)(0)
          val ret = inline(repl)
          logger.debug("  inlined to "+ret)
          ret
        }.getOrElse(p)
      case n => n.nodeMapChildren(inline)
    }

    logger.debug("Checking for fuseable inner comprehensions at "+c.from.map(_._1).mkString(", "))
    val prevSyms = c.from.map(_._1).toSet // Add all syms up front, all refs have to up backwards anyway
    c.from.foreach {
      case t @ (sym, from: Comprehension) if isFuseableInner(sym, from, prevSyms) =>
        logger.debug(sym+" is fuseable inner")
        if(isFuseable(c, from)) {
          logger.debug("Found fuseable generator "+sym+": "+from)
          from.from.foreach { case (s, n) => newFrom += s -> inline(n) }
          for(n <- from.where) newWhere += inline(n)
          for((n, o) <- from.orderBy) newOrderBy += inline(n) -> o
          for(n <- from.groupBy) newGroupBy += inline(n)
          structs += sym -> narrowStructure(from)
          fuse = true
        } else newFrom += ((t._1, inline(t._2)))
      case t =>
        newFrom += ((t._1, inline(t._2)))
    }
    if(fuse) {
      if(logger.isDebugEnabled)
        logger.debug("Fusing Comprehension with new generators "+newFrom.map(_._1).mkString(", ")+":", c)
      val c2 = Comprehension(
        newFrom,
        newWhere ++ c.where.map(inline),
        (c.groupBy.toSeq.map { case n => inline(n) } ++ newGroupBy).headOption,
        c.orderBy.map { case (n, o) => (inline(n), o) } ++ newOrderBy,
        c.select.map { case n => inline(n) },
        c.fetch, c.offset)
      logger.debug("Fused to:", c2)
      c2
    }
    else c
  }

  def select(selects: List[Symbol], base: Node): Vector[Node] = {
    logger.debug("select("+FwdPath.toString(selects)+", "+base+")")
    (selects, base) match {
      //case (s, Union(l, r, _, _, _)) => select(s, l) ++ select(s, r)
      case (Nil, n) => Vector(n)
      case ((s: AnonSymbol) :: t, StructNode(ch)) => select(t, ch.find{ case (s2,_) => s == s2 }.get._2)
      case ((s: ElementSymbol) :: t, ProductNode(ch)) => select(t, ch(s.idx-1))
      case _ => throw new SlickException("Cannot select "+Path.toString(selects.reverse)+" in "+base)
    }
  }

  def narrowStructure(n: Node): Node = n match {
    case Pure(n) => n
    //case Join(_, _, l, r, _, _) => ProductNode(narrowStructure(l), narrowStructure(r))
    //case u: Union => u.copy(left = narrowStructure(u.left), right = narrowStructure(u.right))
    case Comprehension(from, _, _, _, None, _, _) => narrowStructure(from.head._2)
    case Comprehension(_, _, _, _, Some(n), _, _) => narrowStructure(n)
    case n => n
  }
}

object UnionLeft {
  def unapply(n: Node): Option[Node] = n match {
    case u: Union => unapply(u.left)
    case n => Some(n)
  }
}

/** Inject the proper orderings into the RowNumber nodes produced earlier by
  * the resolveFixJoins phase. */
class FixRowNumberOrdering extends Phase {
  val name = "fixRowNumberOrdering"

  def apply(state: CompilerState) = state.map { n =>
    if(state.get(Phase.resolveZipJoins).map(_.hasRowNumber).getOrElse(true))
      ClientSideOp.mapServerSide(n)(ch => fixRowNumberOrdering(ch))
    else {
      logger.debug("No row numbers to fix")
      n
    }
  }

  /** Push ORDER BY into RowNumbers in ordered Comprehensions. */
  def fixRowNumberOrdering(n: Node, parent: Option[Comprehension] = None): Node = (n, parent) match {
    case (r @ RowNumber(_), Some(c)) if !c.orderBy.isEmpty =>
      RowNumber(c.orderBy).nodeTyped(r.nodeType)
    case (c: Comprehension, _) => c.nodeMapScopedChildren {
      case (Some(gen), ch) => fixRowNumberOrdering(ch, None)
      case (None, ch) => fixRowNumberOrdering(ch, Some(c))
    }
    case (n, _) => n.nodeMapChildren(ch => fixRowNumberOrdering(ch, parent), keepType = true)
  }
}
