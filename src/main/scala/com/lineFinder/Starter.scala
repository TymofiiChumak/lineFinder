package com.lineFinder

import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.scene.layout.FlowPane
import javafx.scene.canvas.Canvas
import javafx.scene.canvas.GraphicsContext
import javafx.fxml.FXMLLoader
import javafx.scene.image
import javafx.scene.layout.VBox
import javafx.scene.paint.Color

import java.io.PrintWriter
import java.io.File

import scala.concurrent.{ ExecutionContext, ExecutionContext$, Future, Promise, Await }
import scala.concurrent.duration._

object Starter{
    def main(args: Array[String]){
        Application.launch(classOf[ImagePrinter], args:_*)
    }
}

class ImagePrinter extends Application{
    override def start(stage: Stage){
        val parsed = CommandLineParser.parse(getParameters().getRaw().toArray.map(x => x.toString))        
        val image = ImageLoader.loadImage(parsed("source").toString)
        var gc = printImage(image, "RGB")
        gc.setStroke(Color.GREEN)
        gc.setLineWidth(1)

        val event = new ProcessingEvent()
        def eventListener(event: PrintEventType){
            event match{
                case PrintMono(image) => printImage(image, "Mono")
                case PrintOutline(image) => printImage(image, "Outline")
                case PrintSpace(space) => printSpace(space, 1, 5)
                case PrintLine(coords) => printLine(gc)(coords)
            }
        }
        event.addListener(new ProccesingEventHandler{
            override def handle(event: PrintEventType){
                eventListener(event)
            }
        }) 
        val controllerParams = new ControllerParams(
            image,
            event,
            parsed("threadNum").asInstanceOf[String].toInt,
            parsed("min").asInstanceOf[String].toInt,
            parsed("print").asInstanceOf[String].toBoolean
        )
        val controller = new Controller(controllerParams)
        controller.run()
    }
    
    def printImage(image: Image, printType: String) = {
        val stage = new Stage
        var flowPane = new FlowPane()
        var canvas = new Canvas(image.width, image.height)
        flowPane.getChildren().add(canvas)
        var gc = canvas.getGraphicsContext2D()
        val pw = gc.getPixelWriter()

        def printRGB(){
            for (i <- 0 until image.width; j <- 0 until image.height){
                pw.setArgb(i, j , image.pixels(i)(j))   
            }
        }
        def printMono(){
            for (i <- 0 until image.width; j <- 0 until image.height){
                val color = image.pixels(i)(j)
                pw.setColor(i, j , Color.rgb(color, color, color))   
            }
        }
        def printOutline(){
            for (i <- 0 until image.width; j <- 0 until image.height){
                val color = image.pixels(i)(j) * 255
                pw.setColor(i, j , Color.rgb(color, color, color))   
            }
        }

        printType match{
            case "RGB" => printRGB()
            case "Mono" => printMono()
            case "Outline" => printOutline()
        }
        
        stage.setScene(new Scene(flowPane, image.width, image.height))
        stage.setOnCloseRequest(e => stop());
        stage.show()
        gc
    }

    def printLine(gc: GraphicsContext)(coords: Array[Int]){
        gc.strokeLine(coords(0), coords(1), coords(2), coords(3))
        //writer.write(line(0) + " " + line(1) + " " + line(2) + " " + line(3) + "\n")
    }

    def printSpace(space: ParameterSpace, coefA: Int, coefR: Int){
        val stage = new Stage
        var flowPane = new FlowPane()
        var canvas = new Canvas(space.rangeA / coefA, space.rangeR / coefR)
        flowPane.getChildren().add(canvas)
        var gc = canvas.getGraphicsContext2D()
        
        val pw = gc.getPixelWriter()
        val colors = Array.ofDim[Int](space.rangeA / coefA, space.rangeR / coefR)
        var max = -1
        println((space.rangeA, space.rangeR))
        for (i <- 0 until (space.rangeA / coefA); j <- 0 until (space.rangeR / coefR)){
            var color = 0
            for (k <- 0 until coefA; l <- 0 until coefR){
                color += space.space(i * coefA + k)(j * coefR + l)
            }
            colors(i)(j) = color
            if (color > max) max = color
        }
        for (i <- 0 until (space.rangeA / coefA); j <- 0 until (space.rangeR / coefR)){
            val color = (colors(i)(j).toDouble * (255.0 / max.toDouble)).toInt
            pw.setColor(i, j , Color.rgb(color, color, color))   
        }
        println("max: " + max)
        stage.setScene(new Scene(flowPane, space.rangeA / coefA, space.rangeR / coefR))
        stage.show()
    }

    
}


trait PrintEventType

trait ProccesingEventHandler{
    def handle(event: PrintEventType): Unit
}

class ProcessingEvent(){
    var listeners = Array[ProccesingEventHandler]()

    def addListener(listener: ProccesingEventHandler){
        listeners = listeners :+ listener
    }

    def notify(event: PrintEventType){
        listeners.map(listener => listener.handle(event))
    }
}

case class PrintSpace(val space: ParameterSpace) extends PrintEventType
case class PrintMono(val image: Image) extends PrintEventType
case class PrintOutline(val image: Image) extends PrintEventType
case class PrintLine(val coords: Array[Int]) extends PrintEventType