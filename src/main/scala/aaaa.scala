import scala.concurrent._
import ExecutionContext.Implicits.global
import akka.{ NotUsed, Done }
import akka.actor.ActorSystem

import akka.stream._
import akka.stream.scaladsl._

import GraphDSL.Implicits._

import scala.util.{Success, Failure}

class AAA (val a: Array[(Int, Int)]) extends Iterable[(Int, Int)]{
    def this() = {
        this(Array[(Int, Int)]())
    }
    def insert(index: Int, value: (Int, Int)){
        a(index) = (value)
    }
    class AAAIterator(val aaa: AAA, var index: Int = 0) extends Iterator[(Int, Int)]{
        def next: (Int, Int) = {
            index += 1
            a(index - 1)
        }
        def hasNext = index < a.size
    }
    def iterator = new AAAIterator(this)
}


object BBB{
    
        val a1 = (1 to 10).toArray
    val a2 = (2 to 11).toArray
    val a = new AAA(a1 zip a2)

    def getFuture = {
        Future.sequence {
            for (i <- 1 to 10) yield{
                Future{
                    println(i)
                    a.insert(i - 1, (10 - i, i))
                }
            }
        }
    }

    def wait(future: Future[Any]) = {
        future onComplete{
            case Success(a) => println("success")
            case Failure(a) => println("failure")
        }
    }
}


object CCC{
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val a1 = (1 to 10).toArray
    val a2 = (2 to 11).toArray
    val a = new AAA(a1 zip a2)

    val source: Source[(Int, Int), NotUsed] = Source.fromIterator(() => a.iterator)
    val flow1: Flow[(Int, Int), (Int, Int), NotUsed] = Flow[(Int, Int)].map(
        x => (x._1 * 2, x._2 * 3)
    )
    val flow2: Flow[(Int, Int), (Int, Int), NotUsed] = Flow[(Int, Int)].flatMapConcat(
        x => Source.fromIterator(() => Array((x._1 + 4, x._2  - 3), (x._1 + 4, x._2  - 3)).iterator)
        )
    val sink1: Sink[(Int, Int), Future[Done]] = Sink.foreach(x => println("main: " + x))
    val sink2: Sink[(Int, Int), Future[Done]] = Sink.foreach(x => println("additonnal: " + x))
    val graph = RunnableGraph.fromGraph(GraphDSL.create(){
        implicit builder =>
            val s: Outlet[(Int, Int)] = builder.add(source).out
            val f1: FlowShape[(Int, Int), (Int, Int)] = builder.add(flow1)
            val b: UniformFanOutShape[(Int, Int), (Int, Int)] = builder.add(Broadcast[(Int, Int)](2))
            val f2: FlowShape[(Int, Int), (Int, Int)] = builder.add(flow2)
            val e1: Inlet[(Int, Int)] = builder.add(sink1).in
            val e2: Inlet[(Int, Int)] = builder.add(sink2).in

            s ~> f1
            f1 ~> b
            b ~> e2
            b ~> f2
            f2 ~> e1
            ClosedShape
    })

    def run = graph.run()
}