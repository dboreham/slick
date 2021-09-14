package slick.util

import scala.util.DynamicVariable

object LongStackTraceUtil {

      type StackTrace = Array[StackTraceElement]

      val threadLocalAncestorStackTraces = new DynamicVariable[LongStackTrace](null)

      def getLongStackTrace() = {
            // Make a new object from the current stack trace and whatever is in TLS for ancestors
            val currentStackTrace: LongStackTraceUtil.StackTrace = Thread.currentThread().getStackTrace()
            // TODO: chop the tail off here
            // This has to go in TLS in the child thread
            return new LongStackTrace(threadLocalAncestorStackTraces, currentStackTrace)
      }

      def chainStackTraces(childStackTrace: StackTrace) = {
        // merge and return
        threadLocalAncestorStackTraces.value.get.appendedAll(childStackTrace)
      }
}