package com.lineFinder

import akka.actor.{ ActorSystem, Actor, ActorRef, Props, PoisonPill }
import akka.stream.scaladsl._
import akka.stream._
import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import akka.stream.scaladsl._
import scala.util.{Success, Failure}

import ExecutionContext.Implicits.global
import akka.{ NotUsed, Done }

import GraphDSL.Implicits._

import scala.concurrent.ExecutionContext.Implicits.global

class ControllerParams(val image: Image, 
                       val events: ProcessingEvent,
                       val threadsNum: Int,
                       val min: Int,
                       val print: Boolean)

class Controller(params: ControllerParams){
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    val threadsNum = params.threadsNum.toInt 
    def run() = {
        def getMono(image: Image): Image = {
            val width = params.image.width   
            val height = params.image.height   
            val mono = new Image(height, width)
            Await.ready(Future.sequence(
                for (i <- 0 until threadsNum) yield{
                    val begin = width * i / threadsNum
                    val end = width * (i + 1) / threadsNum
                    Future{
                        mono.getMonoFromImage(image)(begin, end)
                    }
                }
            ), 100.second) 
            mono
        }
        def getOutline(image: Image): Image = {
            val width = image.width   
            val height = image.height   
            val outline = new Image(height, width)
            Await.ready(Future.sequence(
                for (i <- 0 until threadsNum) yield{
                    val begin = width * i / threadsNum
                    val end = width * (i + 1) / threadsNum
                    Future{
                        outline.getOutlineFromImage(image)(begin, end)
                    }
                }
            ), 100.second) 
            outline
        }
        def getTransform(image: Image): HoughTransform = {
            val ht = new HoughTransform(image, params.min)
            val width = image.width
            Await.ready(Future.sequence(
                for (i <- 0 until threadsNum) yield{
                    val begin = width * i / threadsNum
                    val end = width * (i + 1) / threadsNum
                    Future{
                        ht.transform(begin, end)
                    }
                }
            ), 100.second) 
            ht
        }
        def processMaxiums(ht: HoughTransform): SearchMaxium = {
            val width = ht.rangeA
            val height = ht.rangeR
            val sm = new SearchMaxium(ht.space, width, height, params.min)
            Await.ready(Future.sequence(
                for (i <- 0 until threadsNum) yield{
                    val begin = width * i / threadsNum
                    val end = width * (i + 1) / threadsNum
                    Future{
                        sm.process(begin, end)
                    }
                }
            ), 100.second) 
            sm
        }

        val mono: Image = getMono(params.image)
        params.events.notify(PrintMono(mono))
        val outline: Image = getOutline(mono)
        params.events.notify(PrintOutline(outline))
        val ht = getTransform(outline)
        params.events.notify(PrintSpace(ht.getSpace))
        val sm = processMaxiums(ht)
        val source: Source[(Int, Int), NotUsed] = Source.fromIterator(() => sm.iterator)
        var filtered = scala.collection.mutable.Map[Int, Int]()
        val filter: Flow[(Int, Int), (Int, Int), NotUsed] = Flow[(Int, Int)].filter(
            params => ht.filter(filtered)(params)
        )
        val getParametersFlow: Flow[(Int, Int), (Double, Double), NotUsed] = Flow[(Int, Int)].mapAsync(threadsNum)(
            params => Future(ht.getParameters(params))
        )
        val getLinesFlow: Flow[(Double, Double), Array[Int], NotUsed] = Flow[(Double, Double)].flatMapMerge(
            threadsNum,
            params => Source.fromIterator(() => ht.getLines(params).iterator)
        )
        val sink: Sink[Array[Int], Future[Done]] = Sink.foreach(
            line => params.events.notify(PrintLine(line))
        )

        
        val graph = RunnableGraph.fromGraph(GraphDSL.create(){
            implicit builder =>
                val begin: Outlet[(Int, Int)] = builder.add(source).out
                val flow1: FlowShape[(Int, Int), (Double, Double)] = builder.add(getParametersFlow)
                val flow2: FlowShape[(Double, Double), Array[Int]] = builder.add(getLinesFlow)
                val end: Inlet[Array[Int]] = builder.add(sink).in

                begin ~> flow1; flow1 ~> flow2; flow2 ~> end
                ClosedShape
        })
        graph.run()
    }

}

