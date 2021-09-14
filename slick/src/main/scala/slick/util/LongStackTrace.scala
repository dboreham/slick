package slick.util

class LongStackTrace() {


    // holds the stack trace for our parent (we don't yet know our stack trace because we haven't thrown an exception yet)
    // and the stack trace for any grandparent and so on

    val ancestorStackTraces = new Array[LongStackTraceUtil.StackTrace]

    def append(parent: LongStackTrace) = {

    }
    
}