package com.lineFinder

object CommandLineParser{
    @throws(classOf[IllegalArgumentException])
    def parse(args: Array[String]) = {
        val default = collection.mutable.Map[String, String](
            "source" -> "input.txt",
            "min" -> "10",
            "print" -> "false",
            "threadNum" -> "4")
        def loop(lines: List[String], parsed: collection.mutable.Map[String, String]): collection.mutable.Map[String, String] = {
            lines match{
                case Nil => parsed
                case "-o" :: source :: rest => parsed("source") = source; loop(rest, parsed)
                case "-m" :: min :: rest => parsed("min") = min; loop(rest, parsed)
                case "-p" :: rest => parsed("print") = "true"; loop(rest, parsed)
                case "-t" :: threadNum :: rest => parsed("threadNum") = threadNum; loop(rest, parsed)
                case _ => throw new IllegalArgumentException()
            }
        }
        loop(args.toList, default)
    }
}