package slick.util

import scala.util.DynamicVariable
import java.util.Stack

object LongStackTraceUtil {

      type StackTrace = Array[StackTraceElement]
      type LongStackTrace = Array[StackTrace]

      val threadLocalAncestorStackTraces = new DynamicVariable[LongStackTrace](null)

      def getCurrentLongStackTrace(): LongStackTrace = {
            // Make a new object from the current stack trace and whatever is in TLS for ancestors
            val currentStackTrace: StackTrace = Thread.currentThread().getStackTrace()
            // TODO: chop the tail off here
            // This has to go in TLS in the child thread
            return threadLocalAncestorStackTraces.value.appended(currentStackTrace)
      }

      def setAncestorStackTrace(ancestorStackTraces: LongStackTrace) = {
            threadLocalAncestorStackTraces.value = ancestorStackTraces
      }

      def createChainedStackTrace(childStackTrace: StackTrace): StackTrace = {
        // merge and return
        threadLocalAncestorStackTraces.value.flatten.appendedAll(childStackTrace)
      }

      def clearAncestorStackTrace() = {
            threadLocalAncestorStackTraces.value = null
      }
}