package slick.util

import scala.util.DynamicVariable
import java.util.Stack

object LongStackTraceUtil {

      type StackTrace = Array[StackTraceElement]
      type LongStackTrace = Array[StackTrace]

      val threadLocalAncestorStackTraces = new DynamicVariable[LongStackTrace](null)

      def getCurrentLongStackTrace(): LongStackTrace = {
            // Make a new object from the current stack trace and whatever is in TLS for ancestors
            // chop the top off here because it includes this function -- need to remove the top two elements
            val currentStackTrace: StackTrace = Thread.currentThread().getStackTrace().drop(2)
            // This has to go in TLS in the child thread
            threadLocalAncestorStackTraces.value match {
                  case null => Array(currentStackTrace)
                  case _ => {
                        val separatorStackTrace = Array(new StackTraceElement("dummy", "", "", 1))
                        threadLocalAncestorStackTraces.value.prependedAll(Array(currentStackTrace, separatorStackTrace))
                  }
            }
      }

      def setAncestorStackTrace(ancestorStackTraces: LongStackTrace) = {
            threadLocalAncestorStackTraces.value = ancestorStackTraces
      }

      def createChainedStackTrace(childStackTrace: StackTrace): StackTrace = {
        chainStackTraces(threadLocalAncestorStackTraces.value.prepended(childStackTrace)).flatten
      }

      def clearAncestorStackTrace() = {
            threadLocalAncestorStackTraces.value = null
      }

      private def chainStackTraces(traces: LongStackTrace): LongStackTrace = {
            val separatorStackTrace = Array(new StackTraceElement("Context-switch-->>>>", "", "", 1))
            traces.flatMap( x => Array(x, separatorStackTrace))
      }
}